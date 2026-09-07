
import logging
import shutil
import tempfile
from pathlib import Path

import geopandas as gpd
import pandas as pd


PROJECT_ROOT = Path(__file__).parent.parent
DATA_PREPARATION_DIR = PROJECT_ROOT / "sync" / "datapreparation"
SAMPLE_DATA_PATH = DATA_PREPARATION_DIR / "dataclean" / "sample_data.csv"
OUTPUT_DIR = DATA_PREPARATION_DIR / "step3_heatmap"
HEATMAP_DIR = OUTPUT_DIR / "guangzhou_baiduheatmap"
WORKDAY_DIR = HEATMAP_DIR / "workday"
WEEKEND_DIR = HEATMAP_DIR / "weekend"
TMP_DIR = OUTPUT_DIR / ".tmp"
OUTPUT_PATH = OUTPUT_DIR / "heatmap.csv"
OUTPUT_DETAIL_PATH = OUTPUT_DIR / "heatmap_detail.csv"
LOG_PATH = OUTPUT_DIR / "step4.log"
BUFFER_SIZE = 250
CRS_WGS84 = "EPSG:4326"
CRS_UTM = "EPSG:32649"
HOUR_START = 9
HOUR_END = 17

OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
logger = logging.getLogger(__name__)
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
    handlers=[
        logging.FileHandler(LOG_PATH, encoding="utf-8"),
        logging.StreamHandler(),
    ],
    force=True,
)


def _write_csv_atomically(dataframe: pd.DataFrame, output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    temporary_path = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="wb",
            prefix=f".{output_path.stem}-",
            suffix=".tmp",
            dir=output_path.parent,
            delete=False,
        ) as temporary_file:
            temporary_path = Path(temporary_file.name)

        dataframe.to_csv(temporary_path, index=False, encoding="utf-8-sig")
        temporary_path.replace(output_path)
        temporary_path = None
    finally:
        if temporary_path is not None:
            temporary_path.unlink(missing_ok=True)


def create_sample_buffers():
    df = pd.read_csv(SAMPLE_DATA_PATH)
    gdf = gpd.GeoDataFrame(
        df[["sub_id"]], geometry=gpd.points_from_xy(df["lon"], df["lat"]), crs=CRS_WGS84
    )
    gdf = gdf.to_crs(CRS_UTM)
    gdf["geometry"] = gdf.geometry.buffer(BUFFER_SIZE)
    coords = df[["sub_id", "lon", "lat"]].set_index("sub_id")
    return gdf, df["sub_id"].tolist(), coords


def extract_datetime_id(filename):
    stem = Path(filename).stem
    return stem.split("_")[1]


def filter_daytime_files(directory):
    files = []
    for csv_file in sorted(directory.glob("*.csv")):
        datetime_id = extract_datetime_id(csv_file.name)
        hour = int(datetime_id[-2:])
        if HOUR_START <= hour <= HOUR_END:
            files.append(csv_file)
    return files


def process_heatmap_file(heatmap_file, buffers_gdf):
    df = pd.read_csv(heatmap_file, usecols=["wgs84_LNG", "wgs84_LAT", "value"])

    gdf = gpd.GeoDataFrame(
        df[["value"]],
        geometry=gpd.points_from_xy(df["wgs84_LNG"], df["wgs84_LAT"]),
        crs=CRS_WGS84,
    )
    gdf = gdf.to_crs(CRS_UTM)


    joined = gpd.sjoin(gdf, buffers_gdf, how="inner", predicate="within")


    result = joined.groupby("sub_id")["value"].sum().reset_index()

    return result


def main():
    logger.info("========== step4 run started ==========")
    try:

        TMP_DIR.mkdir(parents=True, exist_ok=True)


        logger.info("[1/6] 创建样点缓冲区...")
        buffers_gdf, all_sub_ids, coords = create_sample_buffers()
        logger.info("创建了 %d 个缓冲区", len(buffers_gdf))


        logger.info("[2/6] 筛选白天时段文件...")
        workday_files = filter_daytime_files(WORKDAY_DIR)
        weekend_files = filter_daytime_files(WEEKEND_DIR)
        all_files = workday_files + weekend_files
        logger.info(
            "工作日: %d 个文件, 周末: %d 个文件, 总计: %d",
            len(workday_files),
            len(weekend_files),
            len(all_files),
        )


        logger.info("[3/6] 处理热力图文件...")
        workday_columns = set()
        weekend_columns = set()
        for processed_count, heatmap_file in enumerate(all_files, start=1):
            datetime_id = extract_datetime_id(heatmap_file.name)
            result = process_heatmap_file(heatmap_file, buffers_gdf)
            result = result.rename(columns={"value": datetime_id})

            tmp_file = TMP_DIR / f"{datetime_id}.csv"
            result.to_csv(tmp_file, index=False)

            if heatmap_file.parent == WORKDAY_DIR:
                workday_columns.add(datetime_id)
            else:
                weekend_columns.add(datetime_id)

            logger.info(
                "  [%d/%d] %s", processed_count, len(all_files), heatmap_file.name
            )


        logger.info("[4/6] 合并所有临时文件...")
        all_dfs = []
        for tmp_file in sorted(TMP_DIR.glob("*.csv")):
            df = pd.read_csv(tmp_file).set_index("sub_id")
            all_dfs.append(df)

        sub_id_index = pd.Index(all_sub_ids, name="sub_id")
        merged = pd.concat(all_dfs, axis=1).reindex(sub_id_index).fillna(0)
        merged = merged.copy()
        merged.reset_index(inplace=True)
        logger.info("合并完成，%d 个样点", len(merged))


        logger.info("[5/6] 计算均值...")
        value_columns = [col for col in merged.columns if col != "sub_id"]

        total_file_count = len(workday_columns) + len(weekend_columns)
        workday_file_count = len(workday_columns)
        weekend_file_count = len(weekend_columns)

        workday_cols = [col for col in value_columns if col in workday_columns]
        weekend_cols = [col for col in value_columns if col in weekend_columns]

        merged["total"] = (
            merged[[col for col in value_columns]].sum(axis=1) / total_file_count
        )
        merged["workday"] = (
            merged[workday_cols].sum(axis=1) / workday_file_count if workday_cols else 0
        )
        merged["weekend"] = (
            merged[weekend_cols].sum(axis=1) / weekend_file_count if weekend_cols else 0
        )


        logger.info("[6/6] 输出结果...")
        merged = merged.merge(coords, on="sub_id", how="left")

        output_df = merged[["sub_id", "lon", "lat", "total", "weekend", "workday"]]
        _write_csv_atomically(output_df, OUTPUT_PATH)
        logger.info("汇总版本输出到 %s", OUTPUT_PATH)

        detail_df = merged[
            ["sub_id", "lon", "lat"]
            + sorted(value_columns)
            + ["total", "workday", "weekend"]
        ]
        _write_csv_atomically(detail_df, OUTPUT_DETAIL_PATH)
        logger.info("完整版本输出到 %s", OUTPUT_DETAIL_PATH)


        shutil.rmtree(TMP_DIR)
        logger.info("清理临时目录 %s", TMP_DIR)

        logger.info("========== step4 run completed ==========")
    except Exception:
        logger.exception("========== step4 run failed ==========")
        raise


if __name__ == "__main__":
    main()
