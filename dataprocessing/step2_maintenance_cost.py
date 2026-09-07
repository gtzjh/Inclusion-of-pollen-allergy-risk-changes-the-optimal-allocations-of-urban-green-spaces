
from __future__ import annotations

import math
import tempfile
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_PREPARATION_DIR = PROJECT_ROOT / "sync" / "datapreparation"
INPUT_PATH = DATA_PREPARATION_DIR / "dataclean" / "cleaned_data.csv"
OUTPUT_PATH = DATA_PREPARATION_DIR / "step2_maintenance_cost" / "maintenance_cost.csv"

SUB_ID_COLUMN = "sub_id"
SPECIES_COLUMN = "gbif_accepted_scientific_name"
LIFEFORM_COLUMN = "lifeform"
FAMILY_COLUMN = "gbif_family"
BREAST_DIAMETER_COLUMN = "breast_d_m"
COVER_COLUMN = "cover_m2"
COST_COLUMN = "maintenance_cost"
REQUIRED_COLUMNS = (
    SUB_ID_COLUMN,
    SPECIES_COLUMN,
    LIFEFORM_COLUMN,
    FAMILY_COLUMN,
    BREAST_DIAMETER_COLUMN,
    COVER_COLUMN,
)
OUTPUT_COLUMNS = (
    SUB_ID_COLUMN,
    SPECIES_COLUMN,
    LIFEFORM_COLUMN,
    BREAST_DIAMETER_COLUMN,
    COVER_COLUMN,
    COST_COLUMN,
)
VALID_LIFEFORMS = frozenset({"tree", "shrub", "herb"})
PALM_FAMILY = "Arecaceae"
DIAMETER_TOLERANCE_CM = 1e-5
LONG_TERM_MAINTENANCE_COEFFICIENT = 0.25
MONTHS_PER_YEAR = 12
QUOTA_BASIS = 100
MANAGEMENT_FEE_RATE = 0.13
LABOR_INDEX_2024 = (109.20 + 108.85 + 108.50 + 107.20) / 4
LABOR_FACTOR_2024 = LABOR_INDEX_2024 / 100



NON_PALM_TREE_QUOTAS = (
    (6.0, 474.07, 110.97, 202.96),
    (12.0, 604.98, 193.25, 395.18),
    (20.0, 770.77, 333.21, 699.88),
    (35.0, 983.77, 460.23, 999.98),
    (50.0, 1181.72, 551.16, 1208.55),
    (60.0, 1418.50, 659.83, 1426.32),
)
PALM_TREE_QUOTAS = (
    (20.0, 540.88, 145.83, 289.36),
    (35.0, 646.27, 227.54, 493.34),
    (50.0, 772.78, 333.21, 699.88),
    (70.0, 983.77, 460.23, 999.98),
    (80.0, 1181.72, 551.16, 1208.55),
)
SHRUB_QUOTAS = (
    (80.0, 292.88, 65.38, 161.19),
    (120.0, 343.15, 83.46, 194.33),
    (180.0, 405.99, 102.55, 231.05),
)
TURF_QUOTA = (150.63, 47.19, 118.07)


def _blank_mask(series: pd.Series) -> pd.Series:
    return series.isna() | series.astype("string").str.strip().eq("").fillna(False)


def _validate_and_prepare_input(dataframe: pd.DataFrame) -> pd.DataFrame:
    missing_columns = sorted(set(REQUIRED_COLUMNS) - set(dataframe.columns))
    if missing_columns:
        raise KeyError(f"Missing required columns: {missing_columns}")

    lifeforms = dataframe[LIFEFORM_COLUMN].astype("string")
    invalid_lifeform_mask = lifeforms.isna() | ~lifeforms.isin(VALID_LIFEFORMS)
    if invalid_lifeform_mask.any():
        invalid_counts = lifeforms[invalid_lifeform_mask].value_counts(dropna=False)
        raise ValueError(
            "lifeform must be one of tree, shrub, or herb: "
            f"invalid_rows={int(invalid_lifeform_mask.sum())}, "
            f"values={invalid_counts.to_dict()}"
        )

    prepared = dataframe.copy()
    prepared[BREAST_DIAMETER_COLUMN] = pd.to_numeric(
        prepared[BREAST_DIAMETER_COLUMN], errors="coerce"
    )
    prepared[COVER_COLUMN] = pd.to_numeric(prepared[COVER_COLUMN], errors="coerce")

    relevant_size_masks = {
        BREAST_DIAMETER_COLUMN: lifeforms.eq("tree"),
        COVER_COLUMN: lifeforms.isin(("shrub", "herb")),
    }
    for column, relevant_mask in relevant_size_masks.items():
        values = prepared[column]
        invalid_mask = relevant_mask & (
            values.isna() | ~np.isfinite(values) | values.lt(0)
        )
        if invalid_mask.any():
            invalid_rows = prepared.index[invalid_mask].tolist()
            raise ValueError(
                f"{column} contains missing, non-numeric, negative, or "
                f"non-finite values in {int(invalid_mask.sum())} relevant rows; "
                f"row_indices={invalid_rows[:10]}"
            )

    tree_mask = lifeforms.eq("tree")
    missing_tree_family_mask = tree_mask & _blank_mask(prepared[FAMILY_COLUMN])
    if missing_tree_family_mask.any():
        invalid_rows = prepared.index[missing_tree_family_mask].tolist()
        raise ValueError(
            "Tree rows must have gbif_family: "
            f"invalid_rows={int(missing_tree_family_mask.sum())}, "
            f"row_indices={invalid_rows[:10]}"
        )

    return prepared


def _adjusted_quota_base(
    labor_cost: float, material_cost: float, machinery_cost: float
) -> float:
    adjusted_labor_cost = labor_cost * LABOR_FACTOR_2024
    adjusted_management_fee = MANAGEMENT_FEE_RATE * (
        adjusted_labor_cost + machinery_cost
    )
    return (
        adjusted_labor_cost + material_cost + machinery_cost + adjusted_management_fee
    )


def _quota_for_diameter(
    diameter_cm: float,
    quotas: tuple[tuple[float, float, float, float], ...],
) -> tuple[float, float, float]:
    for upper_cm, labor_cost, material_cost, machinery_cost in quotas:
        if diameter_cm <= upper_cm + DIAMETER_TOLERANCE_CM:
            return labor_cost, material_cost, machinery_cost
    _, labor_cost, material_cost, machinery_cost = quotas[-1]
    return labor_cost, material_cost, machinery_cost


def _annual_cost_from_quota(
    labor_cost: float, material_cost: float, machinery_cost: float
) -> float:
    adjusted_base = _adjusted_quota_base(labor_cost, material_cost, machinery_cost)

    return (
        adjusted_base
        * LONG_TERM_MAINTENANCE_COEFFICIENT
        * MONTHS_PER_YEAR
        / QUOTA_BASIS
    )


def calculate_maintenance_cost(dataframe: pd.DataFrame) -> pd.DataFrame:
    prepared = _validate_and_prepare_input(dataframe)
    costs = pd.Series(np.nan, index=prepared.index, dtype="float64")

    tree_mask = prepared[LIFEFORM_COLUMN].eq("tree")
    palm_tree_mask = tree_mask & prepared[FAMILY_COLUMN].eq(PALM_FAMILY)
    non_palm_tree_mask = tree_mask & ~palm_tree_mask

    for row_index in prepared.index[non_palm_tree_mask]:
        diameter_cm = prepared.at[row_index, BREAST_DIAMETER_COLUMN] * 100
        costs.at[row_index] = _annual_cost_from_quota(
            *_quota_for_diameter(diameter_cm, NON_PALM_TREE_QUOTAS)
        )

    for row_index in prepared.index[palm_tree_mask]:
        diameter_cm = prepared.at[row_index, BREAST_DIAMETER_COLUMN] * 100
        costs.at[row_index] = _annual_cost_from_quota(
            *_quota_for_diameter(diameter_cm, PALM_TREE_QUOTAS)
        )

    shrub_mask = prepared[LIFEFORM_COLUMN].eq("shrub")
    for row_index in prepared.index[shrub_mask]:
        cover_m2 = prepared.at[row_index, COVER_COLUMN]
        diameter_cm = 2 * math.sqrt(cover_m2 * 10000 / math.pi)
        costs.at[row_index] = _annual_cost_from_quota(
            *_quota_for_diameter(diameter_cm, SHRUB_QUOTAS)
        )

    herb_mask = prepared[LIFEFORM_COLUMN].eq("herb")
    turf_annual_cost_per_m2 = _annual_cost_from_quota(*TURF_QUOTA)
    costs.loc[herb_mask] = (
        prepared.loc[herb_mask, COVER_COLUMN] * turf_annual_cost_per_m2
    )

    result = prepared.loc[
        :,
        (
            SUB_ID_COLUMN,
            SPECIES_COLUMN,
            LIFEFORM_COLUMN,
            BREAST_DIAMETER_COLUMN,
            COVER_COLUMN,
        ),
    ].copy()
    result[COST_COLUMN] = costs
    return result.loc[:, OUTPUT_COLUMNS]


def _write_csv_atomically(dataframe: pd.DataFrame, output_path: Path) -> None:
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_dataframe = dataframe.copy()
    output_dataframe[COST_COLUMN] = output_dataframe[COST_COLUMN].map(
        lambda value: "" if pd.isna(value) else f"{value:.4f}"
    )

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
        output_dataframe.to_csv(temporary_path, index=False, encoding="utf-8-sig")
        temporary_path.replace(output_path)
        temporary_path = None
    finally:
        if temporary_path is not None:
            temporary_path.unlink(missing_ok=True)


def main() -> pd.DataFrame:
    dataframe = pd.read_csv(INPUT_PATH, encoding="utf-8")
    result = calculate_maintenance_cost(dataframe)
    _write_csv_atomically(result, OUTPUT_PATH)
    print(f"Wrote {OUTPUT_PATH}: rows={len(result)}")
    return result


if __name__ == "__main__":
    main()
