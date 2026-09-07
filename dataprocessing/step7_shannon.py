
from __future__ import annotations

import math
import tempfile
from pathlib import Path

import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_PREPARATION_DIR = PROJECT_ROOT / "sync" / "datapreparation"
INPUT_PATH = DATA_PREPARATION_DIR / "dataclean" / "cleaned_data.csv"
OUTPUT_PATH = DATA_PREPARATION_DIR / "step7_shannon" / "shannon.csv"
SUB_ID_COLUMN = "sub_id"
SPECIES_COLUMN = "gbif_accepted_scientific_name"
GBIF_STATUS_COLUMN = "gbif_status"
GBIF_RANK_COLUMN = "gbif_rank"
OUTPUT_COLUMNS = (SUB_ID_COLUMN, "shannon")
REQUIRED_COLUMNS = (
    SUB_ID_COLUMN,
    SPECIES_COLUMN,
    GBIF_STATUS_COLUMN,
    GBIF_RANK_COLUMN,
)


def calculate_shannon(dataframe: pd.DataFrame) -> float:
    species_counts = dataframe[SPECIES_COLUMN].value_counts()
    total_rows = len(dataframe)
    shannon = -sum(
        (count / total_rows) * math.log(count / total_rows) for count in species_counts
    )
    return 0.0 if shannon == 0.0 else shannon


def _blank_mask(series: pd.Series) -> pd.Series:
    return series.isna() | series.astype("string").str.strip().eq("").fillna(False)


def _validate_input(dataframe: pd.DataFrame) -> None:
    missing_columns = sorted(set(REQUIRED_COLUMNS) - set(dataframe.columns))
    if missing_columns:
        raise KeyError(f"Missing required columns: {missing_columns}")

    blank_counts = {
        column: int(_blank_mask(dataframe[column]).sum()) for column in REQUIRED_COLUMNS
    }
    invalid_blank_counts = {
        column: count for column, count in blank_counts.items() if count
    }
    if invalid_blank_counts:
        raise ValueError(
            f"Required columns contain missing or blank values: {invalid_blank_counts}"
        )

    status_values = (
        dataframe[GBIF_STATUS_COLUMN].astype("string").str.strip().str.upper()
    )
    invalid_status_mask = status_values.ne("ACCEPTED")
    if invalid_status_mask.any():
        invalid_status_counts = (
            status_values[invalid_status_mask].value_counts().to_dict()
        )
        raise ValueError(
            "GBIF status must be ACCEPTED for every row: "
            f"invalid_rows={int(invalid_status_mask.sum())}, "
            f"values={invalid_status_counts}"
        )

    rank_values = dataframe[GBIF_RANK_COLUMN].astype("string").str.strip().str.lower()
    invalid_rank_mask = rank_values.ne("species")
    if invalid_rank_mask.any():
        invalid_rank_counts = rank_values[invalid_rank_mask].value_counts().to_dict()
        raise ValueError(
            "GBIF rank must be species for every row: "
            f"invalid_rows={int(invalid_rank_mask.sum())}, "
            f"values={invalid_rank_counts}"
        )


def _write_csv_atomically(dataframe: pd.DataFrame, output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    temporary_path: Path | None = None
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


def main() -> pd.DataFrame:
    dataframe = pd.read_csv(INPUT_PATH, encoding="utf-8")
    _validate_input(dataframe)
    shannon_dataframe = (
        dataframe.groupby(SUB_ID_COLUMN, sort=True)
        .apply(calculate_shannon, include_groups=False)
        .rename("shannon")
        .reset_index()
        .loc[:, OUTPUT_COLUMNS]
    )
    _write_csv_atomically(shannon_dataframe, OUTPUT_PATH)
    print(
        f"Wrote {OUTPUT_PATH}: groups={len(shannon_dataframe)} "
        f"input_rows={len(dataframe)}"
    )
    return shannon_dataframe


if __name__ == "__main__":
    main()
