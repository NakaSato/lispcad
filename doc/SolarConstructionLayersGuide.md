# Solar Construction Layers Guide

**Version:** 1.0  
**Last Updated:** May 19, 2025  
**Created By:** LispCAD Team

## Overview

The Solar Construction Layers module provides a comprehensive layer management system for solar construction plans in AutoCAD. This guide explains how to use the module to create standardized layers for solar design projects.

## Features

- Creates a complete set of standardized layers for solar construction plans
- Organizes layers into logical categories (base, civil, electrical, structural, etc.)
- Applies proper colors and linetypes to each layer
- Creates layer groups for easier management (on supported AutoCAD versions)
- Integrates with other Solar Project Tools

## Layer Categories

The Solar Construction Layers module creates the following categories of layers:

| Category | Description | Color Range | Prefix |
|----------|-------------|-------------|--------|
| Base | Core drawing elements | 7-8 | S-BORDER, S-GRID, S-NOTES |
| Existing Conditions | Site boundaries, topography | 8-9 | S-EXIST-* |
| Civil/Site | Grading, drainage, roads | 8-140 | S-CIVIL-* |
| Foundation | Pile and trench foundations | 32-40 | S-FOUND-* |
| Mounting | Rack system and mounting details | 5 | S-MOUNT-* |
| Solar Array | Panel layout and dimensions | 30, 170 | S-ARRAY-* |
| Electrical | Equipment, wiring, grounding | 6, 34, 156 | S-ELEC-* |
| Structural | Beams, columns, bracing | 5 | S-STRUCT-* |
| Staging | Construction areas and access | 51 | S-STAGE-* |
| Details | Section markers and views | 7 | S-DETAIL-* |
| Defects | Marking areas for rework | 1 | S-DEF-* |
| Commissioning | Commissioning zones and sequence | 3 | S-COMM-* |
| As-Built | Documentation of changes | 1-3 | S-ASBUILT-* |
| Fire Safety | Fire access and equipment | 1 | S-FIRE-* |
| Permitting | Boundaries and setbacks | 5 | S-PERMIT-* |
| Environmental | Protected areas and vegetation | 92-150 | S-ENV-* |
| Monitoring | Sensors and weather stations | 40 | S-MON-* |
| Security | Fencing and access control | 8 | S-SEC-* |

## Layer Naming Convention

All layers follow a consistent naming pattern:

```
S-CATEGORY-ELEMENT
```

For example:
- `S-ARRAY-PANELS` - Individual solar panels
- `S-ELEC-DC` - DC wiring and conduits
- `S-CIVIL-ROAD` - Access roads and paths

## Usage Instructions

### Basic Usage

1. Start the Solar Construction Layers generator:
   ```
   Command: CREATESOLARCONSTRUCTIONLAYERS
   ```

2. The command will create all standard solar construction layers with appropriate colors and linetypes.

3. On supported AutoCAD versions, layer groups will be created for easier management.

### Integration with Solar Tools

The Solar Construction Layers command is also available through the Solar Tools menu:

1. Type `SOLARTOOLS` to open the Solar Project Tools menu.
2. Select option B (Create Construction Layers) from the menu.

## Complete Layer List

Below is the complete list of layers created by the Solar Construction Layers module:

### Base Layers
- `S-BORDER` - Drawing border and title block
- `S-GRID` - Site grid reference system
- `S-NOTES` - General notes and annotations

### Existing Conditions
- `S-EXIST-BOUNDARY` - Property and site boundaries
- `S-EXIST-TOPO` - Existing topography
- `S-EXIST-STRUCT` - Existing structures
- `S-EXIST-UTIL` - Existing utilities

### Civil/Site Layers
- `S-CIVIL-GRADE` - Proposed grading and earthwork
- `S-CIVIL-DRAIN` - Drainage features
- `S-CIVIL-ROAD` - Access roads and paths
- `S-CIVIL-FENCE` - Perimeter fencing

### Foundation and Mounting
- `S-FOUND-PILE` - Pile foundations
- `S-FOUND-TRENCH` - Foundation trenches
- `S-FOUND-REBAR` - Reinforcement details
- `S-FOUND-DETAIL` - Foundation details
- `S-MOUNT-RACK` - Mounting rack system
- `S-MOUNT-DETAIL` - Mounting details

### Solar Array Components
- `S-ARRAY-LAYOUT` - Solar array layout
- `S-ARRAY-TABLES` - Module tables/trackers
- `S-ARRAY-PANELS` - Individual solar panels (Color 170)
- `S-ARRAY-DIM` - Array dimensions

### Electrical
- `S-ELEC-EQUIP` - Electrical equipment locations
- `S-ELEC-INV` - Inverters and combiners
- `S-ELEC-DC` - DC wiring and conduits
- `S-ELEC-AC` - AC wiring and conduits
- `S-ELEC-TRENCH` - Electrical trenches
- `S-ELEC-GRND` - Grounding system
- `S-ELEC-DETAIL` - Electrical details

### Structural
- `S-STRUCT-BEAM` - Structural beams
- `S-STRUCT-COL` - Columns
- `S-STRUCT-BRACE` - Bracing elements
- `S-STRUCT-DETAIL` - Structural details

### Staging and Construction
- `S-STAGE-AREA` - Staging areas
- `S-STAGE-ACCESS` - Construction access
- `S-STAGE-TEMP` - Temporary facilities
- `S-STAGE-PHASES` - Construction phasing

### Details and Sections
- `S-DETAIL-MARK` - Section and detail markers
- `S-DETAIL-SEC` - Section views
- `S-DETAIL-CONN` - Connection details

### Defect Layers
- `S-DEF-MARK` - Defect markers
- `S-DEF-REWORK` - Areas requiring rework

### Commissioning
- `S-COMM-ZONES` - Commissioning zones
- `S-COMM-SEQ` - Commissioning sequence

### As-built Documentation
- `S-ASBUILT-CHANGE` - As-built changes
- `S-ASBUILT-ADD` - As-built additions
- `S-ASBUILT-NOTES` - As-built notes

### Fire Safety
- `S-FIRE-ACCESS` - Fire access paths
- `S-FIRE-EQUIP` - Fire protection equipment
- `S-FIRE-ZONE` - Fire zones and boundaries
- `S-FIRE-HYDRANT` - Fire hydrants and water sources

### Permitting and Regulatory
- `S-PERMIT-BOUND` - Permitting boundaries
- `S-PERMIT-SETBACK` - Code-required setbacks
- `S-PERMIT-NOTES` - Permit-related annotations

### Environmental and Vegetation
- `S-ENV-PROTECT` - Environmental protection areas
- `S-ENV-VEGET` - Vegetation and landscaping
- `S-ENV-WATER` - Water bodies and drainage

### Monitoring and Measurement
- `S-MON-SENSOR` - Monitoring sensors and equipment
- `S-MON-WEATHER` - Weather stations
- `S-MON-CAMERA` - Security cameras and monitoring

### Access and Security
- `S-SEC-FENCE` - Security fencing
- `S-SEC-ACCESS` - Security access points
- `S-SEC-GATE` - Gates and entry controls

## Layer Groups

On supported AutoCAD versions, the following layer groups are created:

- Solar Construction
  - Base
  - Existing
  - Civil
  - Foundation
  - Array
  - Electrical
  - Structural
  - Staging
  - Details
  - Defects
  - Commissioning
  - As-Built
  - Fire Safety
  - Permitting
  - Environmental
  - Monitoring
  - Security

## Troubleshooting

If you encounter issues with the Solar Construction Layers module:

1. **Linetype issues**: If certain linetypes are not properly loaded, the command will attempt to load them but may fall back to "Continuous" if they are not available.

2. **Layer description issues**: Some AutoCAD versions may not support layer descriptions. The command will detect this and skip setting descriptions.

3. **Layer group issues**: Layer groups are only supported in newer AutoCAD versions. If your version doesn't support them, the command will automatically skip creating groups.

4. **Loading errors**: If the Solar Construction Layers module fails to load, ensure it is in the correct directory and that it is included in the priority files list in your LispCAD_WindowsLoader.lsp file.

## See Also

- [Solar Project Tools Guide](SolarToolsGuide.md) - Overview of all solar design tools
- [Solar Array Layout Guide](SolarArrayLayoutGuide.md) - Documentation for the solar array layout tool
