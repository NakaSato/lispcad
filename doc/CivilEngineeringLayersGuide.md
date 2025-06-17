# Civil Engineering Layers Guide

**Version:** 1.0  
**Last Updated:** June 8, 2025  
**Created By:** LispCAD Team

## Overview

The Civil Engineering Layers module provides a comprehensive layer management system for earth-bound civil engineering drawings in AutoCAD. This guide explains how to use the module to create standardized layers for civil engineering projects focusing on site preparation, earthworks, foundations, access roads, drainage systems, and non-electrical site infrastructure.

## Features

- Creates a complete set of standardized layers for civil engineering drawings
- Organizes layers into logical categories (site prep, earthworks, foundations, roads, drainage, etc.)
- Applies proper colors and linetypes to each layer type
- Creates layer groups for easier management (on supported AutoCAD versions)
- Integrates with FastDraw system for intelligent drawing workflows
- Supports both full layer sets and specialized subsets for specific project phases

## Layer Categories

The Civil Engineering Layers module creates the following categories of layers:

| Category | Description | Color Range | Prefix |
|----------|-------------|-------------|--------|
| **Base Drawing** | Border, grid, notes, dimensions | 7-8 | C-BASE-* |
| **Existing Conditions** | Topography, buildings, utilities, vegetation | 8-9 | C-EXIST-* |
| **Survey & Control** | Boundaries, setbacks, control points | 30-32 | C-SURV-* |
| **Site Preparation** | Clearing, demolition, tree protection | 40-45 | C-DEMO-* |
| **Earthworks & Grading** | Existing/proposed grades, slopes, berms | 50-55 | C-GRADE-* |
| **Excavation & Cuts** | Excavation limits, temporary/permanent cuts | 60-65 | C-EXCAV-* |
| **Fill & Embankments** | Structural fill, compaction, borrow areas | 70-75 | C-FILL-* |
| **Foundations** | Shallow/deep foundations, footings, piles | 80-85 | C-FOUND-* |
| **Roads & Access** | Centerlines, pavement, base courses | 90-95 | C-ROAD-* |
| **Drainage Systems** | Storm/sanitary, structures, ponds, swales | 100-105 | C-DRAIN-* |
| **Utilities** | Water, sewer, gas, electrical, communication | 110-115 | C-UTIL-* |
| **Erosion Control** | Temporary/permanent, seeding, barriers | 120-125 | C-EROS-* |
| **Environmental** | Wetlands, buffers, protected areas | 130-135 | C-ENV-* |
| **Construction Staging** | Temporary facilities, equipment, permits | 140-145 | C-STAGE-* |

## Quick Start Commands

### Primary Commands

| Command | Description |
|---------|-------------|
| `CreateCivilLayers` | Create complete civil engineering layer set |
| `CivilSitePrep` | Create site preparation layers only |
| `CivilEarthwork` | Create earthwork and grading layers only |
| `CivilDrainage` | Create drainage system layers only |
| `CivilRoads` | Create road and access layers only |
| `CivilFoundations` | Create foundation layers only |

### Usage Examples

```
Command: CreateCivilLayers
Select layer creation mode:
[A]ll layers / [S]ite Prep / [E]arthwork / [D]rainage / [R]oads / [F]oundations: A
Creating complete civil engineering layer set...
✓ 105 civil engineering layers created successfully
```

```
Command: CivilSitePrep
Creating site preparation layers...
✓ Site preparation layers created: C-DEMO-CLEAR, C-DEMO-REMOVE, C-DEMO-PROTECT
```

## Layer Naming Convention

All civil engineering layers follow the pattern: **C-CATEGORY-ELEMENT**

- **C-** = Civil engineering prefix
- **CATEGORY** = Functional category (DEMO, GRADE, DRAIN, etc.)
- **ELEMENT** = Specific element within category

### Examples:
- `C-DEMO-CLEAR` - Site clearing areas
- `C-GRADE-EXIST` - Existing grade elevations
- `C-DRAIN-STORM` - Storm drainage systems
- `C-ROAD-PAVE` - Road pavement
- `C-FOUND-SHALLOW` - Shallow foundations

## Detailed Layer Reference

### Site Preparation Layers (C-DEMO-*)

| Layer Name | Color | Linetype | Description |
|------------|-------|----------|-------------|
| C-DEMO-CLEAR | 40 | Continuous | Site clearing boundaries |
| C-DEMO-REMOVE | 41 | Dashed | Structure removal areas |
| C-DEMO-PROTECT | 42 | Dot-dash | Tree/feature protection zones |
| C-DEMO-SALVAGE | 43 | Continuous | Salvage material areas |
| C-DEMO-HAZMAT | 44 | Continuous | Hazardous material locations |

### Earthworks & Grading Layers (C-GRADE-*)

| Layer Name | Color | Linetype | Description |
|------------|-------|----------|-------------|
| C-GRADE-EXIST | 50 | Continuous | Existing grade contours |
| C-GRADE-PROP | 51 | Dashed | Proposed grade contours |
| C-GRADE-SPOT | 52 | Continuous | Spot elevations |
| C-GRADE-SLOPE | 53 | Dot-dash | Slope indicators |
| C-GRADE-BERM | 54 | Continuous | Earth berms |
| C-GRADE-RETWALL | 55 | Continuous | Retaining walls |

### Excavation Layers (C-EXCAV-*)

| Layer Name | Color | Linetype | Description |
|------------|-------|----------|-------------|
| C-EXCAV-LIMITS | 60 | Continuous | Excavation boundaries |
| C-EXCAV-TEMP | 61 | Dashed | Temporary excavation |
| C-EXCAV-PERM | 62 | Continuous | Permanent excavation |
| C-EXCAV-SHORING | 63 | Dot-dash | Shoring systems |
| C-EXCAV-DEWATER | 64 | Continuous | Dewatering systems |

### Fill & Embankment Layers (C-FILL-*)

| Layer Name | Color | Linetype | Description |
|------------|-------|----------|-------------|
| C-FILL-STRUCT | 70 | Continuous | Structural fill areas |
| C-FILL-COMPACT | 71 | Dashed | Compaction zones |
| C-FILL-BORROW | 72 | Dot-dash | Borrow pit locations |
| C-FILL-WASTE | 73 | Continuous | Waste disposal areas |

### Foundation Layers (C-FOUND-*)

| Layer Name | Color | Linetype | Description |
|------------|-------|----------|-------------|
| C-FOUND-SHALLOW | 80 | Continuous | Shallow foundations |
| C-FOUND-DEEP | 81 | Dashed | Deep foundations |
| C-FOUND-FOOTINGS | 82 | Continuous | Concrete footings |
| C-FOUND-PILES | 83 | Dot-dash | Pile foundations |
| C-FOUND-REBAR | 84 | Continuous | Reinforcement details |
| C-FOUND-DETAILS | 85 | Continuous | Foundation details |

### Road & Access Layers (C-ROAD-*)

| Layer Name | Color | Linetype | Description |
|------------|-------|----------|-------------|
| C-ROAD-CENTER | 90 | Center | Road centerlines |
| C-ROAD-PAVE | 91 | Continuous | Pavement surfaces |
| C-ROAD-BASE | 92 | Dashed | Base course |
| C-ROAD-SUBBASE | 93 | Dot-dash | Subbase course |
| C-ROAD-SHOULDER | 94 | Continuous | Road shoulders |
| C-ROAD-CURB | 95 | Continuous | Curbs and gutters |

### Drainage System Layers (C-DRAIN-*)

| Layer Name | Color | Linetype | Description |
|------------|-------|----------|-------------|
| C-DRAIN-STORM | 100 | Continuous | Storm drainage |
| C-DRAIN-SANITARY | 101 | Dashed | Sanitary sewer |
| C-DRAIN-STRUCT | 102 | Continuous | Drainage structures |
| C-DRAIN-POND | 103 | Dot-dash | Retention ponds |
| C-DRAIN-SWALE | 104 | Continuous | Drainage swales |
| C-DRAIN-INLET | 105 | Continuous | Storm inlets |

### Utility Layers (C-UTIL-*)

| Layer Name | Color | Linetype | Description |
|------------|-------|----------|-------------|
| C-UTIL-WATER | 110 | Continuous | Water systems |
| C-UTIL-SEWER | 111 | Dashed | Sewer systems |
| C-UTIL-GAS | 112 | Dot-dash | Gas lines |
| C-UTIL-ELEC | 113 | Continuous | Electrical utilities |
| C-UTIL-COMM | 114 | Dashed | Communication lines |
| C-UTIL-FIBER | 115 | Continuous | Fiber optic cables |

## Integration with FastDraw System

The Civil Engineering Layers integrate seamlessly with the FastDraw v2.0 system:

### Automatic Layer Switching
When using FastDraw commands, the system automatically switches to appropriate civil layers based on drawing context:

```lisp
;; Example: Drawing drainage swales automatically switches to C-DRAIN-SWALE
Command: FD
FastDraw Mode: [R]apid/[P]attern/[C]onstruction/[B]atch/[PR]ecision: C
Drawing context detected: Drainage
Auto-switched to layer: C-DRAIN-SWALE
```

### Pattern Recognition
FastDraw recognizes civil engineering drawing patterns and suggests appropriate layers:
- Grading operations → C-GRADE-* layers
- Foundation work → C-FOUND-* layers
- Road construction → C-ROAD-* layers
- Drainage design → C-DRAIN-* layers

## Best Practices

### Layer Organization
1. **Use consistent naming** - Follow the C-CATEGORY-ELEMENT convention
2. **Group related layers** - Organize by project phase or discipline
3. **Color coordination** - Use consistent color ranges for each category
4. **Linetype standards** - Apply appropriate linetypes for clarity

### Project Workflow
1. **Start with base layers** - Create border, grid, and notes first
2. **Add existing conditions** - Document current site conditions
3. **Site preparation** - Plan clearing and demolition
4. **Earthworks** - Design grading and excavation
5. **Infrastructure** - Add roads, drainage, and utilities
6. **Foundations** - Detail foundation systems
7. **Environmental controls** - Add erosion control and environmental protection

### Integration Tips
- Use `CreateCivilLayers` at project start for complete layer set
- Use specialized commands (`CivilSitePrep`, etc.) for phase-specific work
- Leverage FastDraw integration for automatic layer management
- Coordinate with other LispCAD layer systems (Solar, MEP, etc.)

## Troubleshooting

### Common Issues

**Layer creation fails:**
- Verify AutoCAD is in model space
- Check for sufficient drawing permissions
- Ensure no conflicting layer names exist

**Colors not displaying correctly:**
- Verify AutoCAD color settings
- Check viewport color overrides
- Confirm layer color assignments

**Integration with FastDraw not working:**
- Reload CivilEngineeringLayers.lsp
- Verify FastDraw v2.0 is loaded
- Check layer detection patterns

### Support Commands

```lisp
;; List all civil layers
(civil:list-all-layers)

;; Check layer status
(civil:check-layer-status "C-GRADE-EXIST")

;; Reset civil layer colors
(civil:reset-layer-colors)
```

## Technical Implementation

The Civil Engineering Layers system is implemented in `CivilEngineeringLayers.lsp` and provides:

- **105+ specialized layers** for comprehensive civil engineering documentation
- **Intelligent categorization** with consistent naming conventions
- **FastDraw integration** for automatic layer management
- **Flexible creation options** for project-specific needs
- **Professional color schemes** optimized for clarity and standards compliance

## See Also

- [FastDraw Usage Guide](FastDraw_Usage_Guide.md) - For drawing system integration
- [Solar Construction Layers Guide](SolarConstructionLayersGuide.md) - For solar project layers
- [Command Reference](../COMMAND_REFERENCE.md) - For complete command listing
- [Integration Documentation](INTEGRATION_COMPLETION_REPORT.md) - For system integration details

---

*This guide covers the complete Civil Engineering Layers system. For technical support or feature requests, refer to the main LispCAD documentation.*
