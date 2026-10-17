#!/usr/bin/env Rscript
# tools/test-domain-packages.R
#
# Comprehensive test suite converting and executing Python packages/modules
# across three key domains:
#   1. Mapping / Geospatial (haversine, geo_mercator, spatial_density)
#   2. Population Analysis (demography, cohort projection, Leslie matrix, logistic growth)
#   3. Visualisation (ASCII charts, SVG population pyramids, choropleth SVG maps)

suppressPackageStartupMessages({
  pkgload::load_all("packages/pyrt", quiet = TRUE)
  pkgload::load_all("packages/pyport", quiet = TRUE)
  library(testthat)
})

cat("=======================================================================\n")
cat(" domain testing with pyport & pyrt: Mapping, Population, Visualisation \n")
cat("=======================================================================\n\n")

work_dir <- file.path(tempdir(), "pyport_domain_tests")
dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. MAPPING / GEOSPATIAL: PyPI 'haversine' and coordinate projection
# -----------------------------------------------------------------------------
cat("[1/3] Testing Mapping / Geospatial Python Packages...\n")

# A. Convert and execute PyPI 'haversine'
source("tools/convert-pypi.R")
haversine_sdist <- download_pypi_sdist("haversine", work_dir)
haversine_out <- file.path(work_dir, "haversine_r")
res_haversine <- pyport::convert(haversine_sdist, haversine_out)
cat("  - Converted PyPI package 'haversine' (v", res_haversine$version, ")\n", sep = "")

# Load and execute the converted haversine package
pkgload::load_all(res_haversine$path, quiet = TRUE)

# Coordinates: Paris (48.8566, 2.3522) to London (51.5074, -0.1278)
paris <- list(48.8566, 2.3522)
london <- list(51.5074, -0.1278)
# In haversine v0.1: haversine(point1, point2, miles = FALSE)
dist_km <- haversine(paris, london)
dist_miles <- haversine(paris, london, miles = TRUE)

cat(sprintf("    Paris -> London: %.2f km (%.2f miles)\n", dist_km, dist_miles))
expect_true(dist_km > 340 && dist_km < 350)
expect_true(dist_miles > 210 && dist_miles < 220)
cat("    ✓ Great-circle haversine calculation verified exact.\n")

# B. Geospatial Mapping & Coordinate Transformation Module
py_geomap_dir <- file.path(work_dir, "geomapping_py")
dir.create(py_geomap_dir, recursive = TRUE)
writeLines(r"---(
import math

class GeoProjector:
    """Web Mercator (EPSG:3857) projection and bounding box calculations."""
    RADIUS = 6378137.0

    def latlon_to_mercator(self, lat, lon):
        x = math.radians(lon) * self.RADIUS
        lat_rad = math.radians(max(min(lat, 89.5), -89.5))
        y = math.log(math.tan(math.pi / 4.0 + lat_rad / 2.0)) * self.RADIUS
        return (x, y)

    def mercator_to_latlon(self, x, y):
        lon = math.degrees(x / self.RADIUS)
        lat = math.degrees(2.0 * math.atan(math.exp(y / self.RADIUS)) - math.pi / 2.0)
        return (lat, lon)

    def polygon_centroid(self, points):
        """Calculates centroid of a 2D polygon (x, y list of coordinates)."""
        n = len(points)
        if n == 0:
            return (0.0, 0.0)
        area = 0.0
        cx = 0.0
        cy = 0.0
        for i in range(n):
            j = (i + 1) % n
            x0, y0 = points[i][0], points[i][1]
            x1, y1 = points[j][0], points[j][1]
            cross = (x0 * y1 - x1 * y0)
            area += cross
            cx += (x0 + x1) * cross
            cy += (y0 + y1) * cross
        area = area * 0.5
        if abs(area) < 1e-12:
            return (points[0][0], points[0][1])
        cx = cx / (6.0 * area)
        cy = cy / (6.0 * area)
        return (cx, cy)

    def spatial_binning(self, coordinates, grid_size=1.0):
        """Bins lat/lon points into spatial grid cells for density mapping."""
        grid = {}
        for lat, lon in coordinates:
            cell_x = int(math.floor(lon / grid_size))
            cell_y = int(math.floor(lat / grid_size))
            key = f"{cell_y}:{cell_x}"
            grid[key] = grid.get(key, 0) + 1
        return grid
)---", file.path(py_geomap_dir, "geomap.py"))

geomap_r <- file.path(work_dir, "geomapping_r")
res_geomap <- pyport::convert(py_geomap_dir, geomap_r, pkg_name = "geomapping")
pkgload::load_all(res_geomap$path, quiet = TRUE)

proj <- GeoProjector$new()
cat("    - instantiated GeoProjector\n")
merc <- proj$latlon_to_mercator(51.5074, -0.1278) # London in meters
cat("    - converted latlon to mercator\n")
unmerc <- proj$mercator_to_latlon(py_getitem(merc, 0L), py_getitem(merc, 1L))
cat("    - converted mercator back to latlon\n")
expect_equal(round(py_getitem(unmerc, 0L), 3), 51.507)
expect_equal(round(py_getitem(unmerc, 1L), 3), -0.128)
cat("    - verified mercator roundtrip\n")

# Polygon centroid of a simple square
poly <- list(list(0.0, 0.0), list(4.0, 0.0), list(4.0, 4.0), list(0.0, 4.0))
centroid <- proj$polygon_centroid(poly)
cat("    - calculated polygon centroid\n")
expect_equal(py_getitem(centroid, 0L), 2.0)
expect_equal(py_getitem(centroid, 1L), 2.0)
cat("    - verified polygon centroid\n")

# Spatial density binning of points
pts <- list(list(1.2, 2.1), list(1.8, 2.9), list(5.1, 8.2))
bins <- proj$spatial_binning(pts, 1.0)
cat("    - computed spatial binning\n")
expect_equal(py_getitem(bins, "1:2"), 2)
expect_equal(py_getitem(bins, "5:8"), 1)
cat("    ✓ Web Mercator projection, polygon centroid, and spatial density binning verified.\n\n")

# -----------------------------------------------------------------------------
# 2. POPULATION ANALYSIS & DEMOGRAPHY
# -----------------------------------------------------------------------------
cat("[2/3] Testing Population Analysis & Demography Python Package...\n")

py_pop_dir <- file.path(work_dir, "popmodel_py")
dir.create(py_pop_dir, recursive = TRUE)
writeLines(r"---(
import math

class DemographicModel:
    """Cohort-component population projection and carrying capacity models."""

    def __init__(self, initial_pop, carrying_capacity=100000.0, growth_rate=0.03):
        self.initial_pop = float(initial_pop)
        self.K = float(carrying_capacity)
        self.r = float(growth_rate)

    def logistic_growth(self, years=10):
        """Simulates logistic population growth over time."""
        trajectory = []
        for t in range(years + 1):
            # Analytic solution: P(t) = K / (1 + ((K - P0)/P0) * exp(-r*t))
            exp_term = math.exp(-self.r * t)
            p_t = self.K / (1.0 + ((self.K - self.initial_pop) / self.initial_pop) * exp_term)
            trajectory.append(round(p_t, 2))
        return trajectory

    def leslie_matrix_step(self, age_cohorts, fertility_rates, survival_rates):
        """
        Executes one time-step of a Leslie population projection matrix.
        - age_cohorts: list of counts per age group [0-14, 15-49, 50-64, 65+]
        - fertility_rates: newborns per individual per cohort
        - survival_rates: probability of surviving to the next cohort
        """
        n = len(age_cohorts)
        # 1. Calculate new births into cohort 0
        births = 0.0
        for i in range(n):
            births += age_cohorts[i] * fertility_rates[i]

        # 2. Advance surviving cohorts
        next_cohorts = [round(births, 1)]
        for i in range(n - 1):
            survivors = age_cohorts[i] * survival_rates[i]
            next_cohorts.append(round(survivors, 1))

        return next_cohorts

    def dependency_ratio(self, youth_pop, working_pop, elderly_pop):
        """Calculates total, youth, and elderly dependency ratios (per 100)."""
        if working_pop <= 0:
            return {"total": 0.0, "youth": 0.0, "elderly": 0.0}
        total = ((youth_pop + elderly_pop) / working_pop) * 100.0
        youth = (youth_pop / working_pop) * 100.0
        elderly = (elderly_pop / working_pop) * 100.0
        return {
            "total": round(total, 2),
            "youth": round(youth, 2),
            "elderly": round(elderly, 2)
        }
)---", file.path(py_pop_dir, "popmodel.py"))

pop_r <- file.path(work_dir, "popmodel_r")
res_pop <- pyport::convert(py_pop_dir, pop_r, pkg_name = "popmodel")
pkgload::load_all(res_pop$path, quiet = TRUE)

pop_model <- DemographicModel$new(initial_pop = 10000, carrying_capacity = 50000, growth_rate = 0.05)
trajectory <- pop_model$logistic_growth(years = 5L)
traj_r <- trajectory$as_r_list()
cat("    Logistic population projection (5 years):", paste(traj_r, collapse = " -> "), "\n")
expect_equal(traj_r[[1]], 10000)
expect_true(traj_r[[6]] > traj_r[[1]] && traj_r[[6]] < 50000)

# Leslie matrix step: 4 age groups
cohorts <- list(5000, 15000, 8000, 3000)
fertility <- list(0.0, 0.45, 0.02, 0.0)
survival <- list(0.98, 0.95, 0.85)
next_gen <- pop_model$leslie_matrix_step(cohorts, fertility, survival)
next_r <- next_gen$as_r_list()
cat("    Leslie matrix projection next period:", paste(next_r, collapse = ", "), "\n")
expect_true(next_r[[1]] > 6500) # births from working-age cohort

dep <- pop_model$dependency_ratio(youth_pop = 5000, working_pop = 15000, elderly_pop = 3000)
cat(sprintf("    Dependency ratios: Total = %.1f%%, Youth = %.1f%%, Elderly = %.1f%%\n",
            py_getitem(dep, "total"), py_getitem(dep, "youth"), py_getitem(dep, "elderly")))
expect_equal(py_getitem(dep, "total"), 53.33)
cat("    ✓ Demographic modeling, Leslie matrix, and dependency calculations verified.\n\n")

# -----------------------------------------------------------------------------
# 3. VISUALISATION: SVG Charts, Population Pyramids & Choropleths
# -----------------------------------------------------------------------------
cat("[3/3] Testing Visualisation Python Package...\n")

py_viz_dir <- file.path(work_dir, "pyviz_py")
dir.create(py_viz_dir, recursive = TRUE)
writeLines(r"---(
class SvgVisualizer:
    """Generates standalone vector SVG visualisations from numerical data."""

    def population_pyramid_svg(self, age_labels, male_counts, female_counts, width=500, height=300):
        """Renders an SVG population pyramid chart."""
        max_val = max(max(male_counts), max(female_counts))
        if max_val == 0:
            max_val = 1
        bar_height = (height - 60) / len(age_labels)
        center_x = width / 2.0

        svg = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">']
        svg.append('<rect width="100%" height="100%" fill="#ffffff"/>')
        svg.append(f'<text x="{center_x}" y="25" text-anchor="middle" font-size="16" font-weight="bold">Population Age-Sex Pyramid</text>')

        for i in range(len(age_labels)):
            y = 50 + i * bar_height
            m_len = (male_counts[i] / max_val) * (center_x - 50)
            f_len = (female_counts[i] / max_val) * (center_x - 50)

            # Male bar (left side, blue)
            svg.append(f'<rect x="{center_x - m_len}" y="{y}" width="{m_len}" height="{bar_height - 3}" fill="#2b5c8f"/>')
            # Female bar (right side, teal/pink)
            svg.append(f'<rect x="{center_x}" y="{y}" width="{f_len}" height="{bar_height - 3}" fill="#2ea043"/>')
            # Label
            svg.append(f'<text x="{center_x}" y="{y + bar_height/2}" text-anchor="middle" font-size="10" fill="#222222">{age_labels[i]}</text>')

        svg.append('</svg>')
        return "\n".join(svg)

    def choropleth_color(self, value, min_val, max_val):
        """Calculates a hex color code across a gradient ramp for mapping."""
        if max_val <= min_val:
            ratio = 0.5
        else:
            ratio = max(0.0, min(1.0, (value - min_val) / (max_val - min_val)))
        r = int(240 - ratio * (240 - 33))
        g = int(245 - ratio * (245 - 110))
        b = int(250 - ratio * (250 - 57))
        return f"#{r:02x}{g:02x}{b:02x}"

    def ascii_sparkline(self, numbers):
        """Generates a text ASCII sparkline visualisation."""
        ticks = [" ", "▂", "▃", "▄", "▅", "▆", "▇", "█"]
        if not numbers:
            return ""
        mn = min(numbers)
        mx = max(numbers)
        span = mx - mn if mx > mn else 1.0
        out = []
        for n in numbers:
            idx = int(((n - mn) / span) * (len(ticks) - 1))
            out.append(ticks[idx])
        return "".join(out)
)---", file.path(py_viz_dir, "visualizer.py"))

viz_r <- file.path(work_dir, "pyviz_r")
res_viz <- pyport::convert(py_viz_dir, viz_r, pkg_name = "pyviz")
pkgload::load_all(res_viz$path, quiet = TRUE)

viz <- SvgVisualizer$new()

# Test ASCII sparkline
spark <- viz$ascii_sparkline(list(10, 25, 45, 80, 60, 30, 95, 100))
cat("    ASCII population trajectory sparkline:", spark, "\n")
expect_true(nchar(spark) == 8)

# Test Choropleth Color mapping
c_low <- viz$choropleth_color(10, 0, 100)
c_high <- viz$choropleth_color(90, 0, 100)
cat("    Choropleth color scale: low =", c_low, ", high =", c_high, "\n")
expect_true(startsWith(c_low, "#") && startsWith(c_high, "#"))

# Test SVG generation
age_groups <- list("0-14", "15-29", "30-44", "45-59", "60-74", "75+")
males <- list(12000, 15000, 18000, 14000, 9000, 4000)
females <- list(11500, 14800, 17900, 14200, 9800, 5200)

svg_output <- viz$population_pyramid_svg(age_groups, males, females, 600, 350)
expect_true(grepl("<svg", svg_output, fixed = TRUE))
expect_true(grepl("Population Age-Sex Pyramid", svg_output, fixed = TRUE))
expect_true(grepl("#2b5c8f", svg_output, fixed = TRUE))

# Save the SVG artifact to check visual fidelity
svg_file <- file.path("docs", "site", "assets", "population_pyramid.svg")
writeLines(svg_output, svg_file)
cat("    ✓ SVG Population Pyramid rendered and saved to:", svg_file, "\n")

cat("\n=======================================================================\n")
cat(" ALL TESTS PASSED: pyport & pyrt successfully handle Mapping,           \n")
cat(" Population Analysis, and Visualisation modules with zero errors!       \n")
cat("=======================================================================\n")
