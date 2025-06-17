# FastDraw v2.0 Usage Guide

**AI-Powered Drawing System for LispCAD**  
**Date:** June 8, 2025  
**Version:** 2.0  

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Drawing Modes Overview](#drawing-modes-overview)
3. [Rapid Mode Guide](#rapid-mode-guide)
4. [Pattern Mode Guide](#pattern-mode-guide)
5. [Construction Mode Guide](#construction-mode-guide)
6. [Batch Mode Guide](#batch-mode-guide)
7. [Precision Mode Guide](#precision-mode-guide)
8. [Advanced Features](#advanced-features)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

---

## Quick Start

### Loading FastDraw

```lisp
;; Load LispCAD (includes FastDraw)
(load "LispCAD_Loader.lsp")

;; Start FastDraw
FastDraw
; or simply
FD
```

### Basic Usage

1. **Run FastDraw**: `FD` or `FastDraw`
2. **Select Mode**: Choose from RAPID, PATTERN, CONSTRUCTION, BATCH, or PRECISION
3. **Follow Prompts**: System provides intelligent guidance
4. **Complete Task**: FastDraw optimizes the drawing process

### Mode Quick Access

```lisp
FDRA    ; Rapid mode
FDPA    ; Pattern mode  
FDCO    ; Construction mode
FDB     ; Batch mode
FDPR    ; Precision mode
```

---

## Drawing Modes Overview

### Mode Selection Guide

| Mode | Best For | Example Use Case |
|------|----------|------------------|
| **RAPID** | Quick sketching | Fast conceptual drawings |
| **PATTERN** | Repetitive elements | Solar panel arrays, grid layouts |
| **CONSTRUCTION** | Reference geometry | Centerlines, axis lines, guides |
| **BATCH** | Multiple objects | Series of circles, text labels |
| **PRECISION** | Exact measurements | Precise dimensioning, coordinates |

---

## Rapid Mode Guide

### Overview
Rapid mode minimizes clicks and maximizes drawing speed with intelligent entity detection.

### Usage

```lisp
FDRA
; System enters rapid drawing mode
; Click points for automatic entity creation
; Minimal prompts, maximum speed
```

### Features

1. **Smart Entity Detection**: System determines best entity type based on context
2. **Auto-Osnap**: Automatic object snap selection
3. **Quick Dimensioning**: Instant dimension creation
4. **Context Awareness**: Adapts to current drawing state

### Example Workflow

1. **Start Rapid Mode**: `FDRA`
2. **Click First Point**: System analyzes context
3. **Click Second Point**: Creates appropriate entity (line, dimension, etc.)
4. **Continue Drawing**: System maintains context
5. **Exit**: Press ESC or switch modes

### Rapid Mode Tips

- **Single Clicks**: For points and nodes
- **Double Clicks**: For entity completion
- **Right-Click**: For mode options
- **ESC**: Exit rapid mode

---

## Pattern Mode Guide

### Overview
Advanced pattern recognition and creation system for repetitive drawing elements.

### Pattern Types

#### Linear Patterns
```lisp
FDPA
; Select: Linear
; Pick base objects
; Define line direction and spacing
; System creates evenly spaced pattern
```

**Use Cases:**
- Solar panel rows
- Fence posts
- Equipment layouts
- Structural elements

#### Rectangular Patterns
```lisp
FDPA
; Select: Rectangular
; Pick base objects
; Define grid parameters (rows, columns, spacing)
; System creates grid array
```

**Use Cases:**
- Solar panel fields
- Window grids
- Parking layouts
- Equipment grids

#### Circular Patterns
```lisp
FDPA
; Select: Circular
; Pick base objects
; Define center point and rotation parameters
; System creates circular array
```

**Use Cases:**
- Radial equipment layouts
- Architectural features
- Mechanical components
- Landscape elements

#### Freeform Patterns
```lisp
FDPA
; Select: Freeform
; Pick base objects
; Define custom path or points
; System creates pattern along path
```

**Use Cases:**
- Irregular layouts
- Terrain following
- Custom arrangements
- Artistic patterns

### Pattern Analysis

```lisp
FDPA
; Select: Analyze Pattern
; Pick existing objects
; System detects pattern and offers options:
; - Extend pattern
; - Modify spacing
; - Create variations
```

### Advanced Pattern Features

1. **Smart Spacing**: Automatic optimal spacing calculation
2. **Constraint Awareness**: Respects drawing boundaries
3. **Layer Management**: Automatic layer assignment
4. **Block Handling**: Intelligent block pattern creation

---

## Construction Mode Guide

### Overview
Automated construction and reference geometry creation for professional drafting.

### Construction Tools

#### Center Lines
```lisp
FDCO
; Select: Center Lines
; Pick objects to analyze
; System creates appropriate centerlines:
; - Object centerlines
; - Symmetry lines
; - Reference axes
```

**Features:**
- Automatic object analysis
- Multiple object support
- Proper line types
- Construction layer assignment

#### Axis Lines
```lisp
FDCO
; Select: Axis Lines
; Pick objects or define manually
; System creates principal axes:
; - X/Y axes
; - Principal object axes
; - Custom axis definitions
```

#### Guide Lines
```lisp
FDCO
; Select: Guide Lines
; Define angle, position, or reference
; System creates reference lines:
; - Angle-based guides
; - Distance-based guides
; - Object-referenced guides
```

#### Offset Lines
```lisp
FDCO
; Select: Offset Lines
; Pick base line/curve
; Specify offset distance
; System creates parallel geometry:
; - Single/multiple offsets
; - Both-side offsets
; - Variable offset distances
```

#### Perpendicular Lines
```lisp
FDCO
; Select: Perpendicular Lines
; Pick base line/curve
; Pick point for perpendicular
; System creates perpendicular construction:
; - Point-to-line perpendiculars
; - Line-to-line perpendiculars
; - Automatic intersections
```

### Construction Layer Management

FastDraw automatically manages construction geometry layers:

- **Construction Lines**: `CONST-LINES`
- **Center Lines**: `CONST-CENTER`  
- **Axis Lines**: `CONST-AXIS`
- **Guide Lines**: `CONST-GUIDE`
- **Dimension Lines**: `CONST-DIM`

---

## Batch Mode Guide

### Overview
Efficient batch operations for repetitive drawing tasks with consistent formatting.

### Batch Operations

#### Multiple Lines
```lisp
FDB
; Select: Multiple Lines
; Enter continuous line mode
; Pick points for connected lines
; Automatic formatting and layer assignment
```

#### Circles at Points
```lisp
FDB
; Select: Circles at Points
; Pick multiple points (or select existing points)
; Specify radius (or use default)
; System creates circles at all points
```

#### Batch Rectangles
```lisp
FDB
; Select: Rectangles
; Define rectangle parameters
; Pick multiple locations
; System creates consistent rectangles
```

#### Block Insertion
```lisp
FDB
; Select: Block Insertion
; Choose block from library
; Pick multiple insertion points
; Automatic scaling and rotation options
```

#### Text Placement
```lisp
FDB
; Select: Text Placement
; Define text content and formatting
; Pick multiple locations
; Consistent text style application
```

### Batch Features

1. **Consistent Formatting**: All batch objects use same properties
2. **Efficient Selection**: Multiple methods for point/location selection
3. **Property Inheritance**: Inherits current layer/style settings
4. **Undo Support**: Single undo for entire batch operation

---

## Precision Mode Guide

### Overview
High-precision drawing tools for exact measurements and coordinate-based construction.

### Precision Tools

#### Exact Distance
```lisp
FDPR
; Select: Exact Distance
; Pick start point
; Enter precise distance: 1547.25
; Enter angle (optional): 45.5
; System creates line with exact specifications
```

#### Angle Precise
```lisp
FDPR
; Select: Angle Precise
; Pick vertex point
; Pick first line point
; Enter exact angle: 30.5
; Pick or enter distance
; System creates precise angular geometry
```

#### Coordinate Entry
```lisp
FDPR
; Select: Coordinate Entry
; Enter coordinates directly:
; X: 1000.5, Y: 2000.25, Z: 0
; System places point/starts geometry at exact location
```

#### Precision Measure
```lisp
FDPR
; Select: Precision Measure
; Pick objects to measure
; System provides exact measurements:
; - Distances
; - Angles  
; - Areas
; - Coordinates
```

#### Grid Align
```lisp
FDPR
; Select: Grid Align
; Define grid parameters
; Pick objects to align
; System snaps objects to precise grid positions
```

### Precision Features

1. **Sub-millimeter Accuracy**: Works with very small tolerances
2. **Coordinate Display**: Real-time coordinate feedback
3. **Measurement Verification**: Automatic measurement confirmation
4. **Grid Integration**: Works with existing grid systems
5. **Unit Conversion**: Automatic unit handling

---

## Advanced Features

### AI-Powered Assistance

FastDraw includes intelligent features:

1. **Context Recognition**: Understands current drawing context
2. **Pattern Detection**: Automatically recognizes existing patterns
3. **Smart Defaults**: Provides intelligent default values
4. **Error Prevention**: Prevents common drawing errors
5. **Optimization**: Optimizes performance for large drawings

### Layer Management

Automatic layer handling:

- **Drawing Layers**: Inherits current layer settings
- **Construction Layers**: Automatic construction layer assignment
- **Pattern Layers**: Intelligent layer organization for patterns
- **Temporary Layers**: Manages temporary geometry

### Performance Optimization

FastDraw is optimized for GstarCAD:

- **Efficient Algorithms**: Optimized for large datasets
- **Memory Management**: Smart memory usage
- **Display Performance**: Optimized screen updates
- **Batch Processing**: Efficient bulk operations

---

## Best Practices

### Mode Selection Strategy

1. **Planning Phase**: Use CONSTRUCTION mode for reference geometry
2. **Layout Phase**: Use PATTERN mode for repetitive elements
3. **Detail Phase**: Use PRECISION mode for exact dimensions
4. **Bulk Creation**: Use BATCH mode for multiple similar objects
5. **Quick Sketching**: Use RAPID mode for conceptual work

### Workflow Integration

1. **Start with Construction**: Create reference geometry first
2. **Add Patterns**: Create repetitive elements second
3. **Batch Operations**: Add bulk objects third
4. **Precision Work**: Finalize with exact measurements
5. **Rapid Sketching**: Use for quick modifications

### Layer Organization

1. **Separate Construction**: Keep construction geometry on separate layers
2. **Pattern Grouping**: Group pattern elements appropriately
3. **Precision Elements**: Keep precise elements on dedicated layers
4. **Batch Consistency**: Ensure batch objects use consistent layers

### Performance Tips

1. **Use Appropriate Mode**: Choose the right mode for the task
2. **Batch Similar Operations**: Group similar operations together
3. **Construction First**: Create reference geometry before details
4. **Regular Cleanup**: Remove unnecessary construction geometry

---

## Troubleshooting

### Common Issues

#### FastDraw Not Loading
```lisp
;; Check if LispCAD is loaded
(if (not (boundp 'FastDraw))
    (load "LispCAD_Loader.lsp"))

;; Verify FastDraw is available
FastDraw
```

#### Mode Not Working
```lisp
;; Reset FastDraw system
(setq *fastdraw-mode* nil)
(setq *fastdraw-state* nil)

;; Restart FastDraw
FD
```

#### Performance Issues
- Use appropriate mode for task size
- Clean up construction geometry regularly
- Work with smaller selections for complex operations
- Use BATCH mode for multiple similar objects

#### Precision Problems
- Check current units and precision settings
- Use PRECISION mode for exact measurements
- Verify coordinate system settings
- Check snap and grid settings

### Getting Help

- **Command Help**: Type `?` in any FastDraw mode
- **Mode Help**: Each mode has built-in help system
- **Examples**: Run example workflows to learn
- **Documentation**: Refer to this guide and command reference

### Support Resources

- **Command Reference**: Main LispCAD command documentation
- **LispCAD Help**: General LispCAD help system
- **Test Commands**: Use LispCAD test commands for diagnostics
- **Community**: LispCAD user community and forums

---

## Integration Examples

### Solar Project Workflow

```lisp
;; 1. Create construction geometry
FDCO
; Create centerlines and axis lines for panel layout

;; 2. Create panel patterns
FDPA
; Create rectangular patterns for panel arrays

;; 3. Add mounting details
FDB
; Batch create mounting points and hardware

;; 4. Precision dimensioning
FDPR
; Add exact dimensions and coordinates

;; 5. Final details
FDRA
; Quick additions and modifications
```

### Architectural Workflow

```lisp
;; 1. Set up grid and axes
FDCO
; Create construction grid and reference lines

;; 2. Create repetitive elements
FDPA
; Windows, columns, structural elements

;; 3. Add details in bulk
FDB
; Doors, fixtures, annotations

;; 4. Precise measurements
FDPR
; Exact dimensions and coordinates

;; 5. Quick modifications
FDRA
; Final adjustments and additions
```

### MEP (Mechanical/Electrical/Plumbing) Workflow

```lisp
;; 1. Equipment layout construction
FDCO
; Create equipment centerlines and mounting references

;; 2. Distribution patterns
FDPA
; Ductwork, conduit, and piping patterns

;; 3. Component placement
FDB
; Bulk placement of devices and fixtures

;; 4. Connection details
FDPR
; Precise connection points and dimensions

;; 5. System annotations
FDRA
; Quick labels and system identifiers
```

---

## Version History

### Version 2.0 (Current)
- Added Pattern Mode with advanced pattern recognition
- Added Construction Mode with automated geometry tools
- Added Batch Mode for efficient bulk operations
- Added Precision Mode for exact measurements
- Enhanced AI-powered assistance
- Improved performance optimization
- Added comprehensive helper functions

### Version 1.0
- Basic FastDraw functionality
- Rapid drawing mode
- Simple entity creation
- Basic optimization

---

**Document Version:** 1.0  
**Last Updated:** June 8, 2025  
**Author:** LispCAD Development Team

For technical support and updates, refer to the main LispCAD documentation and command reference.
