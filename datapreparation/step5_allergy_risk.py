
from __future__ import annotations

import tempfile
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_PREPARATION_DIR = PROJECT_ROOT / "sync" / "datapreparation"
IUGZA_PATH = DATA_PREPARATION_DIR / "step4_iugza" / "iugza.csv"
HEATMAP_PATH = DATA_PREPARATION_DIR / "step3_heatmap" / "heatmap.csv"
ALLERGY_RISK_DIR = DATA_PREPARATION_DIR / "step5_allergy_risk"
VULNERABILITY_PATH = ALLERGY_RISK_DIR / "vulnerability.csv"
OUTPUT_PATH = ALLERGY_RISK_DIR / "allergy_risk.csv"

SITE_COLUMN = "sub_id"
OUTPUT_COLUMNS = (
    SITE_COLUMN,
    "hazard",
    "hazard_normalized",
    "exposure",
    "exposure_normalized",
    "vulnerability_normalized",
    "allergy_risk",
    "allergy_risk_normalized",
)


def _blank_mask(series: pd.Series) -> pd.Series:
    return series.isna() | series.astype("string").str.strip().eq("").fillna(False)


def _require_columns(
    dataframe: pd.DataFrame, required_columns: tuple[str, ...], table_name: str
) -> None:
    missing_columns = sorted(set(required_columns) - set(dataframe.columns))
    if missing_columns:
        raise KeyError(f"{table_name} is missing required columns: {missing_columns}")


def _validate_factor_table(
    dataframe: pd.DataFrame, factor_column: str, table_name: str
) -> pd.DataFrame:
    _require_columns(dataframe, (SITE_COLUMN, factor_column), table_name)

    if _blank_mask(dataframe[SITE_COLUMN]).any():
        raise ValueError(f"{table_name} sub_id values must be nonblank")
    if dataframe[SITE_COLUMN].duplicated().any():
        duplicates = (
            dataframe.loc[dataframe[SITE_COLUMN].duplicated(keep=False), SITE_COLUMN]
            .astype(str)
            .unique()
            .tolist()
        )
        raise ValueError(f"{table_name} sub_id values must be unique: {duplicates}")

    values = pd.to_numeric(dataframe[factor_column], errors="coerce")
    if not (values.notna() & np.isfinite(values)).all():
        raise ValueError(f"{table_name} {factor_column} values must be finite numeric")

    validated = dataframe.loc[:, [SITE_COLUMN, factor_column]].copy()
    validated[factor_column] = values.astype(float)
    return validated


def _min_max_normalize(values: pd.Series) -> pd.Series:
    if values.empty:
        return pd.Series(index=values.index, dtype=float)
    minimum = values.min()
    value_range = values.max() - minimum
    if value_range == 0:
        return pd.Series(0.0, index=values.index, dtype=float)
    return ((values - minimum) / value_range).astype(float)


def calculate_allergy_risk(
    iugza: pd.DataFrame,
    heatmap: pd.DataFrame,
    vulnerability: pd.DataFrame,
) -> pd.DataFrame:
    hazard = _validate_factor_table(iugza, "iugza", "iugza").rename(
        columns={"iugza": "hazard"}
    )
    exposure = _validate_factor_table(heatmap, "total", "heatmap").rename(
        columns={"total": "exposure"}
    )
    vulnerability_factor = _validate_factor_table(
        vulnerability, "scaled_capacity", "vulnerability"
    )

    result = hazard.merge(
        exposure, on=SITE_COLUMN, how="inner", validate="one_to_one"
    ).merge(
        vulnerability_factor,
        on=SITE_COLUMN,
        how="inner",
        validate="one_to_one",
    )
    result = result.sort_values(SITE_COLUMN, kind="stable").reset_index(drop=True)

    result["hazard_normalized"] = _min_max_normalize(result["hazard"])
    result["exposure_normalized"] = _min_max_normalize(result["exposure"])
    result["vulnerability_normalized"] = _min_max_normalize(result["scaled_capacity"])
    result["allergy_risk"] = (
        result["hazard_normalized"]
        * result["exposure_normalized"]
        * result["vulnerability_normalized"]
    )
    result["allergy_risk_normalized"] = _min_max_normalize(result["allergy_risk"])
    return result.loc[:, OUTPUT_COLUMNS]


def _write_csv_atomically(dataframe: pd.DataFrame, output_path: Path) -> None:
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
            dataframe.to_csv(temporary_file, index=False)
        temporary_path.replace(output_path)
        temporary_path = None
    finally:
        if temporary_path is not None:
            temporary_path.unlink(missing_ok=True)


def main() -> pd.DataFrame:
    iugza = pd.read_csv(IUGZA_PATH)
    heatmap = pd.read_csv(HEATMAP_PATH)
    vulnerability = pd.read_csv(VULNERABILITY_PATH)
    result = calculate_allergy_risk(iugza, heatmap, vulnerability)

    _write_csv_atomically(result, OUTPUT_PATH)
    print(
        "Input rows: "
        f"iugza={len(iugza)}, heatmap={len(heatmap)}, "
        f"vulnerability={len(vulnerability)}; "
        f"shared sub_id={len(result)}; output rows={len(result)}"
    )
    print(f"Wrote {OUTPUT_PATH}")
    return result


if __name__ == "__main__":
    main()
