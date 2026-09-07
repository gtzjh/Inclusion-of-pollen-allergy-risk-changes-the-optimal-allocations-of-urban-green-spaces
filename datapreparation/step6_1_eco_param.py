
from __future__ import annotations

import logging
import tempfile
from pathlib import Path

import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_PREPARATION_DIR = PROJECT_ROOT / "sync" / "datapreparation"
ECO_SERVICES_DIR = DATA_PREPARATION_DIR / "step6_eco"
PARAMETER_INPUT_PATH = ECO_SERVICES_DIR / "eco_param.xls"
SPECIES_INPUT_PATH = DATA_PREPARATION_DIR / "dataclean" / "species_data.csv"
OUTPUT_PATH = ECO_SERVICES_DIR / "species_eco_param.csv"
LOG_PATH = ECO_SERVICES_DIR / "step6.log"
PARAMETER_SHEET_NAME = "params"

ACCEPTED_NAME_COLUMN = "gbif_accepted_scientific_name"
GENUS_COLUMN = "gbif_genus"
FAMILY_COLUMN = "gbif_family"
LIFEFORM_COLUMN = "lifeform"
VALID_LIFEFORMS = ("herb", "shrub", "tree")
COMPOSITE_KEY = (LIFEFORM_COLUMN, ACCEPTED_NAME_COLUMN)
MINIMUM_LIFEFORM_DONORS = 3
PARAMETER_COLUMNS = ("LAI", "W_CO2", "W_O2", "W_H2O")
TAXONOMY_COLUMNS = (ACCEPTED_NAME_COLUMN, GENUS_COLUMN, FAMILY_COLUMN)
SPECIES_REQUIRED_COLUMNS = (*TAXONOMY_COLUMNS, LIFEFORM_COLUMN)
PARAMETER_REQUIRED_COLUMNS = (*TAXONOMY_COLUMNS, *PARAMETER_COLUMNS)
BASE_OUTPUT_COLUMNS = (*TAXONOMY_COLUMNS, LIFEFORM_COLUMN)
OUTPUT_COLUMNS = tuple(
    column
    for parameter in PARAMETER_COLUMNS
    for column in (
        parameter,
        f"{parameter}_source",
        f"{parameter}_donor_count",
    )
)
OUTPUT_COLUMNS = (*BASE_OUTPUT_COLUMNS, *OUTPUT_COLUMNS)
DELIVERY_COLUMNS = tuple(
    column for column in OUTPUT_COLUMNS if not column.endswith("_donor_count")
)

logger = logging.getLogger(__name__)


def _configure_logging(log_path: Path) -> None:
    log_path.parent.mkdir(parents=True, exist_ok=True)
    formatter = logging.Formatter("%(asctime)s %(levelname)s %(message)s")
    handlers: list[logging.Handler] = [
        logging.FileHandler(log_path, encoding="utf-8"),
        logging.StreamHandler(),
    ]
    for handler in handlers:
        handler.setFormatter(formatter)

    for handler in logger.handlers:
        handler.close()
    logger.handlers.clear()
    logger.setLevel(logging.INFO)
    logger.propagate = False
    for handler in handlers:
        logger.addHandler(handler)


def _require_columns(
    data: pd.DataFrame, required_columns: tuple[str, ...], source_name: str
) -> None:
    missing_columns = [
        column for column in required_columns if column not in data.columns
    ]
    if missing_columns:
        raise ValueError(f"{source_name}缺少必需字段：{', '.join(missing_columns)}")


def _clean_text(series: pd.Series) -> pd.Series:
    cleaned = series.astype("string").str.strip()
    return cleaned.mask(cleaned.eq(""))


def read_parameter_data(input_path: Path | None = None) -> pd.DataFrame:
    resolved_path = PARAMETER_INPUT_PATH if input_path is None else Path(input_path)
    if not resolved_path.is_file():
        raise FileNotFoundError(f"生态参数文件不存在：{resolved_path}")
    try:
        return pd.read_excel(resolved_path, sheet_name=PARAMETER_SHEET_NAME)
    except ValueError as error:
        raise ValueError(
            f"生态参数文件缺少工作表 {PARAMETER_SHEET_NAME!r}：{resolved_path}"
        ) from error


def read_species_data(input_path: Path | None = None) -> pd.DataFrame:
    resolved_path = SPECIES_INPUT_PATH if input_path is None else Path(input_path)
    if not resolved_path.is_file():
        raise FileNotFoundError(f"物种清单不存在：{resolved_path}")
    return pd.read_csv(resolved_path)


def prepare_parameter_data(parameter_data: pd.DataFrame) -> pd.DataFrame:
    _require_columns(parameter_data, PARAMETER_REQUIRED_COLUMNS, "生态参数表")
    prepared = parameter_data.loc[:, PARAMETER_REQUIRED_COLUMNS].copy()
    for column in TAXONOMY_COLUMNS:
        prepared[column] = _clean_text(prepared[column])

    usable_taxonomy = prepared.loc[:, TAXONOMY_COLUMNS].notna().all(axis=1)
    ignored_count = int((~usable_taxonomy).sum())
    if ignored_count:
        logger.warning("忽略学名、属或科不可用的生态参数记录：count=%d", ignored_count)
    prepared = prepared.loc[usable_taxonomy].copy()

    duplicated_names = prepared.loc[
        prepared[ACCEPTED_NAME_COLUMN].duplicated(keep=False), ACCEPTED_NAME_COLUMN
    ].unique()
    if len(duplicated_names):
        raise ValueError("生态参数表存在重复接受学名：" + ", ".join(duplicated_names))

    for column in PARAMETER_COLUMNS:
        original = prepared[column]
        converted = pd.to_numeric(original, errors="coerce")
        invalid = original.notna() & _clean_text(original).notna() & converted.isna()
        if invalid.any():
            invalid_values = original.loc[invalid].astype(str).unique()
            raise ValueError(
                f"生态参数字段 {column} 包含非数值内容：" + ", ".join(invalid_values)
            )
        prepared[column] = converted

    return prepared.reset_index(drop=True)


def prepare_species_data(species_data: pd.DataFrame) -> pd.DataFrame:
    _require_columns(species_data, SPECIES_REQUIRED_COLUMNS, "物种清单")
    prepared = species_data.loc[:, SPECIES_REQUIRED_COLUMNS].copy()
    for column in TAXONOMY_COLUMNS:
        prepared[column] = _clean_text(prepared[column])
    prepared[LIFEFORM_COLUMN] = _clean_text(prepared[LIFEFORM_COLUMN]).str.lower()

    missing_taxonomy = prepared.loc[:, TAXONOMY_COLUMNS].isna().any(axis=1)
    if missing_taxonomy.any():
        row_numbers = (prepared.index[missing_taxonomy] + 2).tolist()
        raise ValueError(
            "物种清单存在空学名、属或科，CSV 行号：" + ", ".join(map(str, row_numbers))
        )

    missing_lifeform = prepared[LIFEFORM_COLUMN].isna()
    if missing_lifeform.any():
        row_numbers = (prepared.index[missing_lifeform] + 2).tolist()
        raise ValueError(
            "物种清单存在空生活型，CSV 行号：" + ", ".join(map(str, row_numbers))
        )

    invalid_lifeform = ~prepared[LIFEFORM_COLUMN].isin(VALID_LIFEFORMS)
    if invalid_lifeform.any():
        invalid_rows = [
            f"第{index + 2}行={value!r}"
            for index, value in prepared.loc[invalid_lifeform, LIFEFORM_COLUMN].items()
        ]
        raise ValueError(
            "物种清单包含无效生活型，仅允许 herb、shrub、tree："
            + ", ".join(invalid_rows)
        )

    for accepted_name, group in prepared.groupby(
        ACCEPTED_NAME_COLUMN, sort=False, dropna=False
    ):
        conflicting_columns = [
            column
            for column in (GENUS_COLUMN, FAMILY_COLUMN)
            if group[column].nunique(dropna=False) > 1
        ]
        if conflicting_columns:
            raise ValueError(
                f"物种 {accepted_name} 的分类信息冲突："
                + ", ".join(conflicting_columns)
            )

    deduplicated = prepared.drop_duplicates(COMPOSITE_KEY, keep="first")
    ordered_groups = [
        deduplicated.loc[deduplicated[LIFEFORM_COLUMN].eq(lifeform)]
        for lifeform in VALID_LIFEFORMS
    ]
    return pd.concat(ordered_groups, ignore_index=True)


def _map_parameter_donors(
    species_data: pd.DataFrame,
    parameter_data: pd.DataFrame,
    *,
    log_excluded: bool,
) -> pd.DataFrame:
    species_taxonomy = species_data.drop_duplicates(ACCEPTED_NAME_COLUMN).set_index(
        ACCEPTED_NAME_COLUMN
    )
    matched = parameter_data[ACCEPTED_NAME_COLUMN].isin(species_taxonomy.index)
    excluded_count = int((~matched).sum())
    if log_excluded and excluded_count:
        logger.warning("忽略未映射到物种清单的原始参数记录：count=%d", excluded_count)

    matched_parameters = parameter_data.loc[matched].copy()
    for column in (GENUS_COLUMN, FAMILY_COLUMN):
        expected = matched_parameters[ACCEPTED_NAME_COLUMN].map(
            species_taxonomy[column]
        )
        mismatch = matched_parameters[column].ne(expected)
        if mismatch.any():
            details = [
                f"{name}({column}: XLS={actual!r}, 物种清单={wanted!r})"
                for name, actual, wanted in zip(
                    matched_parameters.loc[mismatch, ACCEPTED_NAME_COLUMN],
                    matched_parameters.loc[mismatch, column],
                    expected.loc[mismatch],
                    strict=True,
                )
            ]
            raise ValueError(
                "物种清单与生态参数表的分类信息不一致：" + ", ".join(details)
            )

    lifeform_mapping = species_data.loc[:, [ACCEPTED_NAME_COLUMN, LIFEFORM_COLUMN]]
    expanded = matched_parameters.merge(
        lifeform_mapping,
        on=ACCEPTED_NAME_COLUMN,
        how="inner",
        validate="one_to_many",
    )
    return expanded.loc[:, [LIFEFORM_COLUMN, *PARAMETER_REQUIRED_COLUMNS]]


def _parameter_result(
    species_row: pd.Series,
    parameter_data: pd.DataFrame,
    exact_rows: pd.DataFrame,
    parameter: str,
) -> tuple[float, str, int]:
    lifeform = species_row[LIFEFORM_COLUMN]
    accepted_name = species_row[ACCEPTED_NAME_COLUMN]
    genus = species_row[GENUS_COLUMN]
    family = species_row[FAMILY_COLUMN]

    composite_key = (lifeform, accepted_name)
    if composite_key in exact_rows.index:
        exact_value = exact_rows.at[composite_key, parameter]
        if pd.notna(exact_value):
            return float(exact_value), "exact", 1

    lifeform_pool = parameter_data.loc[parameter_data[LIFEFORM_COLUMN].eq(lifeform)]
    genus_donors = lifeform_pool.loc[
        lifeform_pool[GENUS_COLUMN].eq(genus)
        & lifeform_pool[ACCEPTED_NAME_COLUMN].ne(accepted_name),
        parameter,
    ].dropna()
    if not genus_donors.empty:
        return float(genus_donors.mean()), "genus_mean", len(genus_donors)

    family_donors = lifeform_pool.loc[
        lifeform_pool[FAMILY_COLUMN].eq(family) & lifeform_pool[GENUS_COLUMN].ne(genus),
        parameter,
    ].dropna()
    if not family_donors.empty:
        return float(family_donors.mean()), "family_mean", len(family_donors)

    lifeform_donors = lifeform_pool.loc[
        lifeform_pool[ACCEPTED_NAME_COLUMN].ne(accepted_name),
        [ACCEPTED_NAME_COLUMN, parameter],
    ].dropna(subset=[parameter])
    lifeform_donors = lifeform_donors.drop_duplicates(ACCEPTED_NAME_COLUMN)
    if len(lifeform_donors) >= MINIMUM_LIFEFORM_DONORS:
        return (
            float(lifeform_donors[parameter].mean()),
            "lifeform_mean",
            len(lifeform_donors),
        )

    return float("nan"), "missing", 0


def impute_parameters(
    species_data: pd.DataFrame, parameter_data: pd.DataFrame
) -> pd.DataFrame:
    prepared_species = prepare_species_data(species_data)
    prepared_parameters = prepare_parameter_data(parameter_data)
    mapped_parameters = _map_parameter_donors(
        prepared_species, prepared_parameters, log_excluded=True
    )
    exact_rows = mapped_parameters.set_index(list(COMPOSITE_KEY))
    result = prepared_species.copy()

    for parameter in PARAMETER_COLUMNS:
        parameter_results = [
            _parameter_result(species_row, mapped_parameters, exact_rows, parameter)
            for _, species_row in prepared_species.iterrows()
        ]
        result[parameter] = [item[0] for item in parameter_results]
        result[f"{parameter}_source"] = [item[1] for item in parameter_results]
        result[f"{parameter}_donor_count"] = [item[2] for item in parameter_results]

    return result.loc[:, OUTPUT_COLUMNS]


def build_lifeform_report(
    species_data: pd.DataFrame,
    parameter_data: pd.DataFrame,
    result: pd.DataFrame,
) -> pd.DataFrame:
    lifeform_species = prepare_species_data(species_data)
    prepared_parameters = prepare_parameter_data(parameter_data)
    mapped_parameters = _map_parameter_donors(
        lifeform_species, prepared_parameters, log_excluded=False
    )

    exact_values = mapped_parameters.set_index(list(COMPOSITE_KEY))[
        list(PARAMETER_COLUMNS)
    ]
    before = lifeform_species.join(exact_values, on=list(COMPOSITE_KEY), how="left")
    after = lifeform_species.drop(columns=[GENUS_COLUMN, FAMILY_COLUMN]).merge(
        result.loc[
            :,
            [
                *COMPOSITE_KEY,
                GENUS_COLUMN,
                FAMILY_COLUMN,
                *PARAMETER_COLUMNS,
            ],
        ],
        on=list(COMPOSITE_KEY),
        how="left",
        validate="one_to_one",
    )

    rows: list[dict[str, object]] = []
    lifeforms = lifeform_species[LIFEFORM_COLUMN].drop_duplicates().tolist()
    for lifeform in lifeforms:
        row: dict[str, object] = {LIFEFORM_COLUMN: lifeform}
        for stage, stage_data in (("before", before), ("after", after)):
            subset = stage_data.loc[stage_data[LIFEFORM_COLUMN].eq(lifeform)]
            incomplete = subset.loc[subset.loc[:, PARAMETER_COLUMNS].isna().any(axis=1)]
            row[f"{stage}_incomplete_family_count"] = incomplete[
                FAMILY_COLUMN
            ].nunique()
            row[f"{stage}_incomplete_genus_count"] = incomplete[GENUS_COLUMN].nunique()
            row[f"{stage}_incomplete_species_count"] = incomplete[
                ACCEPTED_NAME_COLUMN
            ].nunique()
        rows.append(row)
    return pd.DataFrame(rows)


def log_summary(
    species_data: pd.DataFrame,
    parameter_data: pd.DataFrame,
    result: pd.DataFrame,
) -> None:
    prepared_species = prepare_species_data(species_data)
    prepared_parameters = prepare_parameter_data(parameter_data)
    mapped_parameters = _map_parameter_donors(
        prepared_species, prepared_parameters, log_excluded=False
    )
    logger.info(
        "输入与去重：物种记录=%d，唯一物种=%d，生活型-物种组合=%d，"
        "原始参数记录=%d，可用原始参数记录=%d，映射后唯一原始供体=%d",
        len(species_data),
        prepared_species[ACCEPTED_NAME_COLUMN].nunique(),
        len(prepared_species),
        len(parameter_data),
        len(prepared_parameters),
        mapped_parameters[ACCEPTED_NAME_COLUMN].nunique(),
    )
    for parameter in PARAMETER_COLUMNS:
        source_counts = result[f"{parameter}_source"].value_counts()
        logger.info(
            "参数来源汇总：parameter=%s exact=%d genus_mean=%d family_mean=%d "
            "lifeform_mean=%d missing=%d",
            parameter,
            int(source_counts.get("exact", 0)),
            int(source_counts.get("genus_mean", 0)),
            int(source_counts.get("family_mean", 0)),
            int(source_counts.get("lifeform_mean", 0)),
            int(source_counts.get("missing", 0)),
        )
        for lifeform in VALID_LIFEFORMS:
            lifeform_result = result.loc[result[LIFEFORM_COLUMN].eq(lifeform)]
            lifeform_sources = lifeform_result[f"{parameter}_source"].value_counts()
            raw_donor_count = mapped_parameters.loc[
                mapped_parameters[LIFEFORM_COLUMN].eq(lifeform)
                & mapped_parameters[parameter].notna(),
                ACCEPTED_NAME_COLUMN,
            ].nunique()
            logger.info(
                "参数与生活型来源：parameter=%s lifeform=%s raw_unique_donors=%d "
                "exact=%d genus_mean=%d family_mean=%d lifeform_mean=%d missing=%d",
                parameter,
                lifeform,
                raw_donor_count,
                int(lifeform_sources.get("exact", 0)),
                int(lifeform_sources.get("genus_mean", 0)),
                int(lifeform_sources.get("family_mean", 0)),
                int(lifeform_sources.get("lifeform_mean", 0)),
                int(lifeform_sources.get("missing", 0)),
            )

    report = build_lifeform_report(species_data, parameter_data, result)
    for row in report.to_dict(orient="records"):
        logger.info(
            "生活型参数不完整统计：lifeform=%s "
            "before_family=%d before_genus=%d before_species=%d "
            "after_family=%d after_genus=%d after_species=%d",
            row[LIFEFORM_COLUMN],
            row["before_incomplete_family_count"],
            row["before_incomplete_genus_count"],
            row["before_incomplete_species_count"],
            row["after_incomplete_family_count"],
            row["after_incomplete_genus_count"],
            row["after_incomplete_species_count"],
        )

    complete_count = int(result.loc[:, PARAMETER_COLUMNS].notna().all(axis=1).sum())
    no_parameter_count = int(result.loc[:, PARAMETER_COLUMNS].isna().all(axis=1).sum())
    partial_count = len(result) - complete_count - no_parameter_count
    logger.info(
        "参数完整度：complete=%d partial=%d none=%d",
        complete_count,
        partial_count,
        no_parameter_count,
    )


def write_output_atomically(data: pd.DataFrame, output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    temporary_path: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8-sig",
            newline="",
            prefix=f".{output_path.stem}-",
            suffix=".tmp",
            dir=output_path.parent,
            delete=False,
        ) as temporary_file:
            temporary_path = Path(temporary_file.name)
            data.to_csv(temporary_file, index=False)
        temporary_path.replace(output_path)
        temporary_path = None
    finally:
        if temporary_path is not None:
            temporary_path.unlink(missing_ok=True)


def main() -> pd.DataFrame:
    _configure_logging(LOG_PATH)
    logger.info("========== step6 开始执行 ==========")
    try:
        species_data = read_species_data()
        parameter_data = read_parameter_data()
        result = impute_parameters(species_data, parameter_data)
        log_summary(species_data, parameter_data, result)
        write_output_atomically(result.loc[:, DELIVERY_COLUMNS], OUTPUT_PATH)
        logger.info("输出完成：path=%s rows=%d", OUTPUT_PATH, len(result))
        logger.info("========== step6 执行完成 ==========")
        return result
    except Exception:
        logger.exception("========== step6 执行失败，保留已有输出 ==========")
        raise


if __name__ == "__main__":
    main()
