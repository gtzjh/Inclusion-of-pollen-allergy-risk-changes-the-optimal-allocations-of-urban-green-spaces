
from __future__ import annotations

import logging
import os
import tempfile
from decimal import Decimal, InvalidOperation
from pathlib import Path

import numpy as np
import pandas as pd

PROJECT_ROOT = Path(__file__).resolve().parents[1]
DATA_PREPARATION_DIR = PROJECT_ROOT / "sync" / "datapreparation"
ECO_SERVICES_DIR = DATA_PREPARATION_DIR / "step6_eco"
OBSERVATION_PATH = DATA_PREPARATION_DIR / "dataclean" / "cleaned_data.csv"
PARAMETER_PATH = ECO_SERVICES_DIR / "species_eco_param.csv"
OUTPUT_PATH = ECO_SERVICES_DIR / "eco_services.csv"



AVERAGE_TEMPERATURE_C = 23.1
CAL_TO_MJ = 4.18e-6
VALID_LIFEFORMS = ("tree", "shrub", "herb")

SITE_COLUMN = "sub_id"
LIFEFORM_COLUMN = "lifeform"
SPECIES_COLUMN = "gbif_accepted_scientific_name"
COVER_COLUMN = "cover_m2"
COMPOSITE_KEY = (LIFEFORM_COLUMN, SPECIES_COLUMN)
PARAMETER_COLUMNS = ("LAI", "W_CO2", "W_H2O")
OBSERVATION_COLUMNS = (
    SITE_COLUMN,
    LIFEFORM_COLUMN,
    SPECIES_COLUMN,
    COVER_COLUMN,
)
PARAMETER_INPUT_COLUMNS = (*COMPOSITE_KEY, *PARAMETER_COLUMNS)
SERVICE_COLUMNS = (
    "ES_LA_tree",
    "ES_CO2_tree",
    "ES_Q_tree",
    "ES_LA_shrub",
    "ES_CO2_shrub",
    "ES_Q_shrub",
    "ES_CO2_herb",
    "ES_Q_herb",
    "ES_LA",
    "ES_CO2",
    "ES_Q",
)
OUTPUT_COLUMNS = (SITE_COLUMN, *SERVICE_COLUMNS)

_INT64_MIN = np.iinfo(np.int64).min
_INT64_MAX = np.iinfo(np.int64).max

logger = logging.getLogger(__name__)


def _require_columns(
    data: pd.DataFrame, required_columns: tuple[str, ...], source_name: str
) -> None:
    bom_columns = [column for column in data.columns if column.startswith("\ufeff")]
    if bom_columns:
        raise ValueError(f"{source_name} contains BOM-prefixed columns: {bom_columns}")
    missing_columns = [
        column for column in required_columns if column not in data.columns
    ]
    if missing_columns:
        raise ValueError(
            f"{source_name} is missing required columns: {missing_columns}"
        )


def _blank_mask(values: pd.Series) -> pd.Series:
    return values.isna() | values.astype("string").str.strip().eq("").fillna(False)


def _row_details(data: pd.DataFrame, invalid: pd.Series, column: str) -> str:
    details = []
    for index, value in data.loc[invalid, column].items():
        details.append(f"input row {index!r}={value!r}")
    return ", ".join(details)


def _key_details(data: pd.DataFrame, invalid: pd.Series) -> str:
    details = []
    for index, row in data.loc[invalid, list(COMPOSITE_KEY)].iterrows():
        key = tuple(row[column] for column in COMPOSITE_KEY)
        details.append(f"input row {index!r} key={key!r}")
    return ", ".join(details)


def read_observation_data(input_path: Path | str | None = None) -> pd.DataFrame:
    resolved_path = OBSERVATION_PATH if input_path is None else Path(input_path)
    data = pd.read_csv(
        resolved_path,
        encoding="utf-8-sig",
        usecols=list(OBSERVATION_COLUMNS),
    )
    _require_columns(data, OBSERVATION_COLUMNS, "observation data")
    return data.loc[:, OBSERVATION_COLUMNS]


def read_parameter_data(input_path: Path | str | None = None) -> pd.DataFrame:
    resolved_path = PARAMETER_PATH if input_path is None else Path(input_path)
    data = pd.read_csv(
        resolved_path,
        encoding="utf-8-sig",
        usecols=list(PARAMETER_INPUT_COLUMNS),
    )
    _require_columns(data, PARAMETER_INPUT_COLUMNS, "parameter data")
    return data.loc[:, PARAMETER_INPUT_COLUMNS]


def _normalize_sub_ids(data: pd.DataFrame, source_name: str) -> pd.Series:
    normalized: list[int] = []
    invalid = pd.Series(False, index=data.index)
    for index, value in data[SITE_COLUMN].items():
        try:
            if pd.isna(value) or isinstance(value, (bool, np.bool_)):
                raise InvalidOperation
            decimal_value = Decimal(str(value).strip())
            if (
                not decimal_value.is_finite()
                or decimal_value != decimal_value.to_integral_value()
                or decimal_value < _INT64_MIN
                or decimal_value > _INT64_MAX
            ):
                raise InvalidOperation
            normalized.append(int(decimal_value))
        except (InvalidOperation, ValueError):
            invalid.at[index] = True
            normalized.append(0)

    if invalid.any():
        raise ValueError(
            f"{source_name} {SITE_COLUMN} must contain finite int64 values: "
            f"{_row_details(data, invalid, SITE_COLUMN)}"
        )
    return pd.Series(normalized, index=data.index, dtype="int64")


def validate_observation_data(observations: pd.DataFrame) -> pd.DataFrame:
    _require_columns(observations, OBSERVATION_COLUMNS, "observation data")
    validated = observations.loc[:, OBSERVATION_COLUMNS].copy()

    for column in COMPOSITE_KEY:
        invalid = _blank_mask(validated[column])
        if invalid.any():
            raise ValueError(
                f"observation data {column} must be nonblank: "
                f"{_row_details(validated, invalid, column)}"
            )

    invalid_lifeforms = ~validated[LIFEFORM_COLUMN].isin(VALID_LIFEFORMS)
    if invalid_lifeforms.any():
        raise ValueError(
            "observation data lifeform must be one of "
            f"{VALID_LIFEFORMS}: "
            f"{_row_details(validated, invalid_lifeforms, LIFEFORM_COLUMN)}"
        )

    validated[SITE_COLUMN] = _normalize_sub_ids(validated, "observation data")
    cover = pd.to_numeric(validated[COVER_COLUMN], errors="coerce")
    valid_cover = cover.notna() & np.isfinite(cover) & cover.ge(0)
    if not valid_cover.all():
        invalid = ~valid_cover
        raise ValueError(
            "observation data cover_m2 must contain finite nonnegative values: "
            f"{_row_details(validated, invalid, COVER_COLUMN)}"
        )
    validated[COVER_COLUMN] = cover.astype(float)
    return validated


def validate_parameter_data(parameters: pd.DataFrame) -> pd.DataFrame:
    _require_columns(parameters, PARAMETER_INPUT_COLUMNS, "parameter data")
    validated = parameters.loc[:, PARAMETER_INPUT_COLUMNS].copy()

    for column in COMPOSITE_KEY:
        invalid = _blank_mask(validated[column])
        if invalid.any():
            raise ValueError(
                f"parameter data {column} must be nonblank: "
                f"{_row_details(validated, invalid, column)}"
            )

    duplicated = validated.duplicated(list(COMPOSITE_KEY), keep=False)
    if duplicated.any():
        raise ValueError(
            "parameter data composite keys must be unique: "
            f"{_key_details(validated, duplicated)}"
        )

    for column in PARAMETER_COLUMNS:
        values = pd.to_numeric(validated[column], errors="coerce")
        valid = values.notna() & np.isfinite(values) & values.gt(0)
        if not valid.all():
            invalid = ~valid
            raise ValueError(
                f"parameter data {column} must contain finite positive values: "
                f"{_row_details(validated, invalid, column)}"
            )
        validated[column] = values.astype(float)
    return validated


def join_ecological_parameters(
    observations: pd.DataFrame, parameters: pd.DataFrame
) -> pd.DataFrame:
    parameter_keys = pd.MultiIndex.from_frame(parameters.loc[:, COMPOSITE_KEY])
    observation_keys = pd.MultiIndex.from_frame(observations.loc[:, COMPOSITE_KEY])
    matched = pd.Series(observation_keys.isin(parameter_keys), index=observations.index)
    if not matched.all():
        raise ValueError(
            "observation composite keys are missing from parameter data: "
            f"{_key_details(observations, ~matched)}"
        )

    observation_count = len(observations)
    joined = observations.merge(
        parameters,
        on=list(COMPOSITE_KEY),
        how="left",
        validate="many_to_one",
        sort=False,
    )
    if len(joined) != observation_count:
        raise ValueError(
            "parameter join changed the observation row count: "
            f"before={observation_count}, after={len(joined)}"
        )
    missing_parameters = joined.loc[:, PARAMETER_COLUMNS].isna().any(axis=1)
    if missing_parameters.any():
        raise ValueError(
            "parameter join produced missing calculation parameters: "
            f"{_key_details(joined, missing_parameters)}"
        )
    return joined


def calculate_record_services(joined: pd.DataFrame) -> pd.DataFrame:
    calculated = joined.copy()
    calculated["leaf_area"] = calculated[COVER_COLUMN] * calculated["LAI"]
    calculated["ES_CO2_record"] = calculated["leaf_area"] * calculated["W_CO2"]
    calculated["ES_H2O_record"] = calculated["leaf_area"] * calculated["W_H2O"]
    latent_heat_cal_per_g = 597 - 0.57 * AVERAGE_TEMPERATURE_C
    calculated["ES_Q_record"] = (
        calculated["ES_H2O_record"] * latent_heat_cal_per_g * CAL_TO_MJ
    )
    return calculated


def aggregate_by_sub_id(records: pd.DataFrame) -> pd.DataFrame:
    sites = (
        records[SITE_COLUMN]
        .drop_duplicates()
        .sort_values(kind="stable")
        .reset_index(drop=True)
    )
    result = pd.DataFrame({SITE_COLUMN: sites.astype("int64")})
    grouped = records.groupby(
        [SITE_COLUMN, LIFEFORM_COLUMN], sort=False, observed=True
    )[["leaf_area", "ES_CO2_record", "ES_Q_record"]].sum()

    for lifeform in VALID_LIFEFORMS:
        try:
            layer = grouped.xs(lifeform, level=LIFEFORM_COLUMN)
        except KeyError:
            layer = pd.DataFrame(columns=grouped.columns)
        if lifeform != "herb":
            result[f"ES_LA_{lifeform}"] = (
                result[SITE_COLUMN].map(layer["leaf_area"]).fillna(0.0)
            )
        result[f"ES_CO2_{lifeform}"] = (
            result[SITE_COLUMN].map(layer["ES_CO2_record"]).fillna(0.0)
        )
        result[f"ES_Q_{lifeform}"] = (
            result[SITE_COLUMN].map(layer["ES_Q_record"]).fillna(0.0)
        )

    result["ES_LA"] = result["ES_LA_tree"] + result["ES_LA_shrub"]
    result["ES_CO2"] = (
        result["ES_CO2_tree"] + result["ES_CO2_shrub"] + result["ES_CO2_herb"]
    )
    result["ES_Q"] = result["ES_Q_tree"] + result["ES_Q_shrub"] + result["ES_Q_herb"]
    return result.loc[:, OUTPUT_COLUMNS]


def validate_output(output: pd.DataFrame) -> pd.DataFrame:
    if tuple(output.columns) != OUTPUT_COLUMNS:
        raise ValueError(
            "output columns do not match the required schema: "
            f"expected={list(OUTPUT_COLUMNS)}, actual={list(output.columns)}"
        )
    validated = output.copy()
    validated[SITE_COLUMN] = _normalize_sub_ids(validated, "output")
    if validated[SITE_COLUMN].duplicated().any():
        duplicates = validated.loc[
            validated[SITE_COLUMN].duplicated(keep=False), SITE_COLUMN
        ].tolist()
        raise ValueError(f"output sub_id values must be unique: {duplicates}")
    if not validated[SITE_COLUMN].is_monotonic_increasing:
        raise ValueError("output sub_id values must be sorted in ascending order")

    for column in SERVICE_COLUMNS:
        values = pd.to_numeric(validated[column], errors="coerce")
        valid = values.notna() & np.isfinite(values) & values.ge(0)
        if not valid.all():
            invalid = ~valid
            raise ValueError(
                f"output {column} must contain finite nonnegative values: "
                f"{_row_details(validated, invalid, column)}"
            )
        validated[column] = values.astype(float)

    identities = {
        "ES_LA": validated["ES_LA_tree"] + validated["ES_LA_shrub"],
        "ES_CO2": (
            validated["ES_CO2_tree"]
            + validated["ES_CO2_shrub"]
            + validated["ES_CO2_herb"]
        ),
        "ES_Q": (
            validated["ES_Q_tree"] + validated["ES_Q_shrub"] + validated["ES_Q_herb"]
        ),
    }
    for total_column, expected in identities.items():
        matches = np.isclose(validated[total_column], expected, rtol=1e-12, atol=1e-12)
        if not matches.all():
            invalid = pd.Series(~matches, index=validated.index)
            raise ValueError(
                f"output {total_column} does not equal its lifeform subtotals: "
                f"{_row_details(validated, invalid, total_column)}"
            )
    return validated


def write_output_atomically(
    output: pd.DataFrame, output_path: Path | str | None = None
) -> None:
    resolved_path = OUTPUT_PATH if output_path is None else Path(output_path)
    resolved_path.parent.mkdir(parents=True, exist_ok=True)
    temporary_path: Path | None = None
    try:
        with tempfile.NamedTemporaryFile(
            dir=resolved_path.parent,
            delete=False,
            mode="w",
            encoding="utf-8",
            newline="",
        ) as temporary_file:
            temporary_path = Path(temporary_file.name)
            output.to_csv(temporary_file, index=False)
            temporary_file.flush()
            os.fsync(temporary_file.fileno())
        os.replace(temporary_path, resolved_path)
        temporary_path = None
    finally:
        if temporary_path is not None and temporary_path.exists():
            temporary_path.unlink()


def main() -> pd.DataFrame:
    observations = read_observation_data()
    parameters = read_parameter_data()
    logger.info(
        "Read observation rows=%d and parameter keys=%d",
        len(observations),
        len(parameters),
    )

    observations = validate_observation_data(observations)
    parameters = validate_parameter_data(parameters)
    joined = join_ecological_parameters(observations, parameters)
    logger.info("Matched observation rows=%d of %d", len(joined), len(observations))
    lifeform_counts = joined[LIFEFORM_COLUMN].value_counts()
    logger.info(
        "Observation rows by lifeform: tree=%d, shrub=%d, herb=%d",
        int(lifeform_counts.get("tree", 0)),
        int(lifeform_counts.get("shrub", 0)),
        int(lifeform_counts.get("herb", 0)),
    )

    records = calculate_record_services(joined)
    output = validate_output(aggregate_by_sub_id(records))
    logger.info("Aggregated sub_id count=%d", len(output))
    write_output_atomically(output)
    logger.info("Wrote ecological services to %s", OUTPUT_PATH)
    return output


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    main()
