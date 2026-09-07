
from __future__ import annotations

import tempfile
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_PREPARATION_DIR = PROJECT_ROOT / "sync" / "datapreparation"
OBSERVATIONS_PATH = DATA_PREPARATION_DIR / "dataclean" / "cleaned_data.csv"
IUGZA_DIR = DATA_PREPARATION_DIR / "step4_iugza"
AP_PATH = IUGZA_DIR / "ap.csv"
PE_PATH = IUGZA_DIR / "pe.csv"
PPP_PATH = IUGZA_DIR / "ppp.csv"
OUTPUT_PATH = IUGZA_DIR / "iugza.csv"

SITE_COLUMN = "sub_id"
SUBPLOT_COLUMN = "subsub_id"
SPECIES_COLUMN = "gbif_accepted_scientific_name"
LIFEFORM_COLUMN = "lifeform"
COVER_COLUMN = "cover_m2"
HEIGHT_COLUMN = "height_m"
LIFEFORMS = ("tree", "shrub", "herb")
OUTPUT_COLUMNS = (
    SITE_COLUMN,
    "tree_iugza",
    "shrub_iugza",
    "herb_iugza",
    "iugza",
    "iugza_normalized",
)
OBSERVATION_COLUMNS = (
    SITE_COLUMN,
    SUBPLOT_COLUMN,
    SPECIES_COLUMN,
    LIFEFORM_COLUMN,
    COVER_COLUMN,
    HEIGHT_COLUMN,
)
REFERENCE_VOLUME = 378.0


def _blank_mask(series: pd.Series) -> pd.Series:
    return series.isna() | series.astype("string").str.strip().eq("").fillna(False)


def _require_columns(
    dataframe: pd.DataFrame, required_columns: tuple[str, ...], table_name: str
) -> None:
    missing_columns = sorted(set(required_columns) - set(dataframe.columns))
    if missing_columns:
        raise KeyError(f"{table_name} is missing required columns: {missing_columns}")


def _validate_integer_parameter(
    dataframe: pd.DataFrame,
    parameter: str,
    minimum: int,
    maximum: int,
) -> pd.DataFrame:
    _require_columns(dataframe, (SPECIES_COLUMN, parameter), parameter.upper())
    if _blank_mask(dataframe[SPECIES_COLUMN]).any():
        raise ValueError(f"{parameter.upper()} species keys must be nonblank")
    if dataframe[SPECIES_COLUMN].duplicated().any():
        duplicates = sorted(
            dataframe.loc[
                dataframe[SPECIES_COLUMN].duplicated(keep=False), SPECIES_COLUMN
            ]
            .astype(str)
            .unique()
            .tolist()
        )
        raise ValueError(
            f"{parameter.upper()} species keys must be unique: {duplicates}"
        )

    values = pd.to_numeric(dataframe[parameter], errors="coerce")
    valid = (
        values.notna()
        & np.isfinite(values)
        & values.eq(np.floor(values))
        & values.between(minimum, maximum)
    )
    if not valid.all():
        raise ValueError(
            f"{parameter.upper()} must contain integers from {minimum} to {maximum}"
        )

    validated = dataframe.loc[:, [SPECIES_COLUMN, parameter]].copy()
    validated[parameter] = values.astype(int)
    return validated


def _validate_inputs(
    observations: pd.DataFrame,
    ap: pd.DataFrame,
    pe: pd.DataFrame,
    ppp: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    _require_columns(observations, OBSERVATION_COLUMNS, "observations")
    observations = observations.copy()

    blank_required = {
        column: int(_blank_mask(observations[column]).sum())
        for column in (SITE_COLUMN, SPECIES_COLUMN)
        if _blank_mask(observations[column]).any()
    }
    if blank_required:
        raise ValueError(
            f"Observation site and species values must be nonblank: {blank_required}"
        )

    lifeforms = observations[LIFEFORM_COLUMN].astype("string").str.strip().str.lower()
    invalid_lifeforms = ~lifeforms.isin(LIFEFORMS)
    if invalid_lifeforms.any():
        invalid_values = sorted(
            observations.loc[invalid_lifeforms, LIFEFORM_COLUMN]
            .astype("string")
            .fillna("<missing>")
            .unique()
            .tolist()
        )
        raise ValueError(
            f"Observation lifeform must be tree, shrub, or herb: {invalid_values}"
        )
    observations[LIFEFORM_COLUMN] = lifeforms

    subplot_rows = observations[LIFEFORM_COLUMN].isin(("shrub", "herb"))
    if _blank_mask(observations.loc[subplot_rows, SUBPLOT_COLUMN]).any():
        raise ValueError("Shrub and herb observations must have nonblank subsub_id")

    ap = _validate_integer_parameter(ap, "ap", 0, 4)
    pe = _validate_integer_parameter(pe, "pe", 0, 3)
    ppp = _validate_integer_parameter(ppp, "ppp", 1, 3)

    observed_species = set(observations[SPECIES_COLUMN])
    ap_species = set(ap[SPECIES_COLUMN])
    missing_ap = sorted(observed_species - ap_species)
    if missing_ap:
        raise ValueError(f"Every observed species must have AP: {missing_ap}")

    positive_ap_species = set(ap.loc[ap["ap"].gt(0), SPECIES_COLUMN])
    pe_species = set(pe[SPECIES_COLUMN])
    ppp_species = set(ppp[SPECIES_COLUMN])
    if pe_species != positive_ap_species:
        raise ValueError(
            "PE species must exactly match positive-AP species: "
            f"missing={sorted(positive_ap_species - pe_species)}, "
            f"extra={sorted(pe_species - positive_ap_species)}"
        )
    if ppp_species != positive_ap_species:
        raise ValueError(
            "PPP species must exactly match positive-AP species: "
            f"missing={sorted(positive_ap_species - ppp_species)}, "
            f"extra={sorted(ppp_species - positive_ap_species)}"
        )

    return observations, ap, pe, ppp


def _tree_effective_height(height: pd.Series) -> pd.Series:
    conditions = (
        height.le(4),
        height.le(8),
        height.le(12),
        height.le(16),
    )
    return pd.Series(
        np.select(conditions, (2.0, 6.0, 10.0, 14.0), default=18.0),
        index=height.index,
        dtype=float,
    )


def _min_max_normalize(values: pd.Series) -> pd.Series:
    if values.empty:
        return pd.Series(index=values.index, dtype=float)
    minimum = values.min()
    value_range = values.max() - minimum
    if value_range == 0:
        return pd.Series(0.0, index=values.index, dtype=float)
    return ((values - minimum) / value_range).astype(float)


def calculate_iugza(
    observations: pd.DataFrame,
    ap: pd.DataFrame,
    pe: pd.DataFrame,
    ppp: pd.DataFrame,
) -> pd.DataFrame:
    observations, ap, pe, ppp = _validate_inputs(observations, ap, pe, ppp)

    sites = (
        observations.loc[:, SITE_COLUMN]
        .drop_duplicates()
        .sort_values(kind="stable")
        .reset_index(drop=True)
    )
    result = pd.DataFrame({SITE_COLUMN: sites})
    for lifeform in LIFEFORMS:
        result[f"{lifeform}_iugza"] = 0.0

    subplot_counts = (
        observations.loc[
            observations[LIFEFORM_COLUMN].isin(("shrub", "herb")),
            [SITE_COLUMN, LIFEFORM_COLUMN, SUBPLOT_COLUMN],
        ]
        .groupby([SITE_COLUMN, LIFEFORM_COLUMN], sort=False, dropna=False)[
            SUBPLOT_COLUMN
        ]
        .nunique()
    )

    participating = observations.merge(
        ap, on=SPECIES_COLUMN, how="left", validate="many_to_one"
    )
    participating = participating.loc[participating["ap"].gt(0)].copy()
    if participating.empty:
        result["iugza"] = 0.0
        result["iugza_normalized"] = 0.0
        return result.loc[:, OUTPUT_COLUMNS]
    participating = participating.merge(
        pe, on=SPECIES_COLUMN, how="left", validate="many_to_one"
    ).merge(ppp, on=SPECIES_COLUMN, how="left", validate="many_to_one")

    cover = pd.to_numeric(participating[COVER_COLUMN], errors="coerce")
    valid_cover = cover.notna() & np.isfinite(cover) & cover.ge(0)
    if not valid_cover.all():
        raise ValueError(
            "Positive-AP observations must have finite nonnegative cover_m2"
        )
    participating[COVER_COLUMN] = cover

    height = pd.to_numeric(participating[HEIGHT_COLUMN], errors="coerce")
    height_required = participating[LIFEFORM_COLUMN].isin(("tree", "shrub"))
    valid_height = height.notna() & np.isfinite(height) & height.gt(0)
    if not valid_height.loc[height_required].all():
        raise ValueError(
            "Positive-AP tree and shrub observations must have finite positive height_m"
        )
    participating[HEIGHT_COLUMN] = height
    participating["vpa"] = (
        participating["ap"] * participating["pe"] * participating["ppp"]
    )

    for lifeform in LIFEFORMS:
        layer = participating.loc[participating[LIFEFORM_COLUMN].eq(lifeform)].copy()
        if layer.empty:
            continue
        if lifeform == "tree":
            effective_height = _tree_effective_height(layer[HEIGHT_COLUMN])
            sampled_area = pd.Series(100.0, index=layer.index)
        elif lifeform == "shrub":
            effective_height = layer[HEIGHT_COLUMN]
            layer_subplot_counts = subplot_counts.xs(lifeform, level=LIFEFORM_COLUMN)
            sampled_area = 25.0 * layer[SITE_COLUMN].map(layer_subplot_counts)
        else:
            effective_height = pd.Series(0.25, index=layer.index)
            layer_subplot_counts = subplot_counts.xs(lifeform, level=LIFEFORM_COLUMN)
            sampled_area = layer[SITE_COLUMN].map(layer_subplot_counts).astype(float)

        layer["weighted_volume"] = layer["vpa"] * layer[COVER_COLUMN] * effective_height
        numerators = layer.groupby(SITE_COLUMN, sort=False)["weighted_volume"].sum()
        areas = sampled_area.groupby(layer[SITE_COLUMN], sort=False).first()
        scores = numerators / (REFERENCE_VOLUME * areas)
        result[f"{lifeform}_iugza"] = (
            result[SITE_COLUMN].map(scores).fillna(0.0).astype(float)
        )

    result["iugza"] = result.loc[:, [f"{name}_iugza" for name in LIFEFORMS]].sum(axis=1)
    result["iugza_normalized"] = _min_max_normalize(result["iugza"])
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
    result = calculate_iugza(
        pd.read_csv(OBSERVATIONS_PATH),
        pd.read_csv(AP_PATH),
        pd.read_csv(PE_PATH),
        pd.read_csv(PPP_PATH),
    )
    _write_csv_atomically(result, OUTPUT_PATH)
    print(f"Wrote {OUTPUT_PATH}: sites={len(result)}")
    return result


if __name__ == "__main__":
    main()
