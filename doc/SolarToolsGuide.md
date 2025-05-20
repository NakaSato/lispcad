# LispCAD Solar Project Tools

## Overview

The Solar Project Tools module is a comprehensive set of CAD tools designed for solar PV system design and documentation within AutoCAD. These tools help automate common solar design tasks, optimize system layouts, analyze sun paths and radiation impacts, and generate professional documentation.

## Available Commands

### Solar Array Design Tools
- **SolarArray** - Create arrays of solar panels with proper spacing
- **OptimizeArray** - Find optimal array settings (tilt, azimuth, spacing) for your location

### Solar Analysis Tools
- **SunPath** - Generate sun path diagrams and shadow projections
- **SolarRadiation** - Analyze solar radiation on tilted surfaces

### Solar Engineering Tools
- **SolarSetback** - Calculate code-required setbacks for solar installations
- **SolarStrings** - Design electrical string connections
- **SolarInfoBlock** - Create system information tables with production estimates

### Component Libraries
- **SolarLib** - Insert standard solar components (panels, inverters, etc.)

### Drawing Utilities
- **CreateScale** - Add scale indicators to plans (enhanced with more scales)
- **UnitScale** - Scale multiple objects simultaneously
- **CreateSolarConstructionLayers** - Generate a comprehensive layer structure for solar construction plans

## Getting Started

1. Load all Solar CAD tools by typing at the AutoCAD command prompt:
```
(load-solar-cad-tools)
```

2. Access all solar design functions through the main menu:
```
SolarTools
```

3. Set up standard solar layers:
```
CreateSolarLayers
```

## How To Use Solar Tools

### Basic Usage

1. **SolarArray**
   - Type `SolarArray` at the command prompt
   - Select the roof or base surface for the array
   - Enter the panel dimensions or select from library
   - Specify array parameters (rows, columns, tilt angle)
   - Choose mounting type (flush, tilted, or ground mount)
   - The command will generate the complete panel array with proper spacing

2. **OptimizeArray**
   - Type `OptimizeArray` at the command prompt
   - Enter your project location (latitude/longitude)
   - Specify site constraints (available area, roof pitch)
   - Review the optimization results showing recommended:
     - Panel tilt angle
     - Azimuth orientation
     - Row spacing for minimal shading
     - Expected energy yield

3. **SunPath**
   - Type `SunPath` at the command prompt
   - Select the target surface or area
   - Specify date and time range for analysis
   - Choose visualization type (diagram, animation, or both)
   - Review the generated sun path arcs and shadow projections

### Advanced Usage

1. **SolarRadiation**
   - Type `SolarRadiation` at the command prompt
   - Select surfaces to analyze
   - Enter location data and time period
   - Choose radiation model (simple or detailed)
   - Review the heat map visualization showing kWh/m² per area

2. **SolarSetback**
   - Type `SolarSetback` at the command prompt
   - Select the roof perimeter
   - Choose local code requirements or enter custom values
   - The command generates the required setback lines with dimensions

3. **SolarStrings**
   - Type `SolarStrings` at the command prompt
   - Select panels to include in strings
   - Specify inverter parameters or select from library
   - Review the generated string connections and electrical diagram

4. **SolarInfoBlock**
   - Type `SolarInfoBlock` at the command prompt
   - Enter project information (client, address, etc.)
   - Select the solar array for automatic data extraction
   - Choose table style and placement location
   - The command generates a complete information table with system specs and production estimates

5. **CreateSolarConstructionLayers**
   - Type `CreateSolarConstructionLayers` at the command prompt
   - The command automatically creates a comprehensive set of standardized layers for solar construction plans
   - Layers are organized into logical categories (base, civil, electrical, structural, etc.)
   - Each layer is assigned an appropriate color and linetype
   - On supported AutoCAD versions, layer groups are created for easier management
   - For full details, see the [Solar Construction Layers Guide](SolarConstructionLayersGuide.md)

### Tips for Optimal Results

- Always start with `OptimizeArray` to establish best system parameters
- Use `SunPath` to identify potential shading obstacles
- Combine `SolarRadiation` with `OptimizeArray` for maximum energy yield
- Save common configurations as templates using the `SaveSolarTemplate` command
- Export results to reports using `ExportSolarReport` for client presentations

## Workflow Example

1. Create standardized layers with `CreateSolarConstructionLayers`
2. Analyze the best position for your array with `OptimizeArray`
3. Create the array layout with `SolarArray`
4. Check shadow impact with `SunPath`
5. Analyze solar radiation with `SolarRadiation`
6. Add setback lines with `SolarSetback`
7. Design string connections with `SolarStrings`
8. Add components with `SolarLib`
9. Create an information block with `SolarInfoBlock`

## Enhanced Features (May 19, 2025 Update)

- **Improved SunPathAnalysis.lsp** - More accurate solar calculations with better visualization
- **Added SolarRadiation Analysis** - Heat map visualization of solar radiation on surfaces
- **Enhanced OptimizeArray Tool** - Detailed recommendations for array configuration
- **Production Estimates** - Added to SolarInfoBlock based on system configuration and location
- **Extended Scale Options** - More architectural and enlargement scales for solar drawings
- **New Solar Construction Layers Module** - Comprehensive layer management system with standardized layers for all aspects of solar construction documentation
- **Updated Panel Color** - Changed S-ARRAY-PANELS layer color to 170 (blue) for better visibility in drawings

Created: May 19, 2025
Last Updated: May 19, 2025
