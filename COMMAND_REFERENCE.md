# LispCAD Command Reference

**Version:** 3.0  
**Date:** June 8, 2025  
**Comprehensive guide to all available commands and functionality**

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Solar Project Tools](#solar-project-tools)
3. [Civil Engineering Layers](#civil-engineering-layers)
4. [Drawing Management](#drawing-management)
5. [Navigation & Zoom](#navigation--zoom)
6. [Layer Management](#layer-management)
7. [Core Aliases](#core-aliases)
8. [Component Libraries](#component-libraries)
9. [System Utilities](#system-utilities)
10. [Testing & Diagnostics](#testing--diagnostics)
11. [Configuration Management](#configuration-management)
12. [Command Index](#command-index)

---

## Quick Start

### Loading LispCAD
```lisp
;; Load all LispCAD components
(load "LispCAD_Loader.lsp")

;; Load specific modules
(load-solar-cad-tools)          ; Solar tools only
(LoadLispCADAll)                ; Everything
```

### Essential Commands
| Command | Description | Category |
|---------|-------------|----------|
| `SolarGCR` | Ground Coverage Ratio calculator | Solar |
| `SolarTools` | Main solar tools menu | Solar |
| `FastDraw` / `FD` | AI-powered drawing system | Drawing |
| `BF` | Bring objects to front | Drawing |
| `ZZ` | Zoom to selected objects | Navigation |
| `LC` | Make object's layer current | Layer |

---

## Solar Project Tools

### Primary Solar Commands

#### `SolarGCR` / `GCR` / `GroundCoverageRatio`
**Interactive Ground Coverage Ratio Calculator**

```lisp
SolarGCR
```

**Features:**
- Step-by-step panel specification input
- Multiple input methods for ground area
- Real-time GCR calculation and analysis
- Standards compliance checking
- Optional results table generation

**Workflow:**
1. Select panel type or define custom specifications
2. Configure array layout (panel count, arrangement)
3. Define ground area (direct input, rectangular bounds, or boundary selection)
4. Review comprehensive GCR analysis
5. Optionally create results table in drawing

#### `SolarTools`
**Main Solar Project Tools Menu**

```lisp
SolarTools
```

**Menu Options:**
- A. Ground Coverage Ratio Calculator (`SolarGCR`)
- B. Create Construction Layers (`CreateSolarConstructionLayers`)
- C. Solar Array Layout (`SolarArray`)
- D. Optimize Array Configuration (`OptimizeArray`)
- E. Sun Path Analysis (`SunPath`)
- F. Solar Radiation Analysis (`SolarRadiation`)
- G. Solar Setback Calculator (`SolarSetback`)
- H. Solar String Layout (`SolarStrings`)
- I. Solar Component Library (`SolarLib`)
- J. Solar Information Block (`SolarInfoBlock`)

### Solar Array Design

#### `SolarArray`
**Interactive solar array layout with automatic GCR calculation**

```lisp
SolarArray
```

**Features:**
- Predefined panel configurations
- Custom panel specifications
- Automatic spacing calculations
- Real-time GCR monitoring
- Drawing placement and annotation

#### `OptimizeArray`
**Array optimization tools** *(Implementation planned)*

```lisp
OptimizeArray
```

### Solar Analysis Tools

#### `SunPath`
**Sun path analysis and visualization** *(Implementation planned)*

```lisp
SunPath
```

#### `SolarRadiation`
**Solar radiation analysis** *(Implementation planned)*

```lisp
SolarRadiation
```

#### `SolarSetback`
**Solar setback calculator** *(Implementation planned)*

```lisp
SolarSetback
```

### Solar Documentation

#### `SolarInfoBlock`
**Create solar system information block**

```lisp
SolarInfoBlock
```

**Features:**
- System specifications template
- GCR integration
- Customizable information fields
- Professional formatting

#### `SolarDocs`
**Generate comprehensive solar documentation**

```lisp
SolarDocs
```

**Generates:**
- Installation guides
- Technical specifications
- GCR analysis reports
- System documentation

### Solar System Management

#### `LoadSolarTools`
**Manual loading of solar tools**

```lisp
LoadSolarTools
```

#### `ReloadSolarTools`
**Force reload all solar tools**

```lisp
ReloadSolarTools
```

#### `SolarConfig`
**Interactive configuration management**

```lisp
SolarConfig
```

**Functions:**
- Display current settings
- Modify configuration parameters
- Save/load configuration files
- Reset to defaults

---

## Civil Engineering Layers

### Overview
The Civil Engineering Layers module provides comprehensive layer management for earth-bound civil engineering projects including site preparation, earthworks, foundations, roads, drainage systems, and infrastructure.

**Key Features:**
- 105+ specialized civil engineering layers
- Organized into logical categories with consistent naming (C-CATEGORY-ELEMENT)
- Professional color schemes and linetype standards
- FastDraw integration for automatic layer switching
- Support for site development, infrastructure, and industrial projects

### Primary Civil Engineering Commands

#### `CreateCivilLayers` - Complete Civil Layer Setup
**Create comprehensive civil engineering layer set**

```lisp
CreateCivilLayers
```

**Interactive Options:**
- **[A]ll layers** - Complete 105+ layer set for comprehensive projects
- **[S]ite Prep** - Site preparation layers only (clearing, demolition, protection)
- **[E]arthwork** - Earthwork and grading layers (grades, excavation, fill)
- **[D]rainage** - Drainage system layers (storm, structures, ponds, swales)
- **[R]oads** - Road and access layers (centerlines, pavement, base)
- **[F]oundations** - Foundation layers (shallow, deep, footings, details)

**Example:**
```
Command: CreateCivilLayers
Select layer creation mode:
[A]ll layers / [S]ite Prep / [E]arthwork / [D]rainage / [R]oads / [F]oundations: A
Creating complete civil engineering layer set...
✓ 105 civil engineering layers created successfully
```

#### `CivilSitePrep` - Site Preparation Layers
**Create site preparation and demolition layers**

```lisp
CivilSitePrep
```

**Creates Layers:**
- `C-DEMO-CLEAR` - Site clearing boundaries
- `C-DEMO-REMOVE` - Structure removal areas  
- `C-DEMO-PROTECT` - Tree/feature protection zones
- `C-DEMO-SALVAGE` - Salvage material areas
- `C-DEMO-HAZMAT` - Hazardous material locations

#### `CivilEarthwork` - Earthwork and Grading Layers
**Create earthwork, grading, and excavation layers**

```lisp
CivilEarthwork
```

**Creates Layers:**
- **Grading:** `C-GRADE-EXIST`, `C-GRADE-PROP`, `C-GRADE-SPOT`, `C-GRADE-SLOPE`
- **Excavation:** `C-EXCAV-LIMITS`, `C-EXCAV-TEMP`, `C-EXCAV-PERM`, `C-EXCAV-SHORING`
- **Fill:** `C-FILL-STRUCT`, `C-FILL-COMPACT`, `C-FILL-BORROW`, `C-FILL-WASTE`

#### `CivilDrainage` - Drainage System Layers
**Create comprehensive drainage and stormwater management layers**

```lisp
CivilDrainage
```

**Creates Layers:**
- `C-DRAIN-STORM` - Storm drainage systems
- `C-DRAIN-SANITARY` - Sanitary sewer systems
- `C-DRAIN-STRUCT` - Drainage structures (manholes, inlets)
- `C-DRAIN-POND` - Retention/detention ponds
- `C-DRAIN-SWALE` - Drainage swales and bioretention
- `C-DRAIN-INLET` - Storm inlets and grates

#### `CivilRoads` - Road and Access Layers
**Create road, pavement, and access infrastructure layers**

```lisp
CivilRoads
```

**Creates Layers:**
- `C-ROAD-CENTER` - Road centerlines
- `C-ROAD-PAVE` - Pavement surfaces
- `C-ROAD-BASE` - Base course materials
- `C-ROAD-SUBBASE` - Subbase course
- `C-ROAD-SHOULDER` - Road shoulders
- `C-ROAD-CURB` - Curbs and gutters

#### `CivilFoundations` - Foundation Layers
**Create foundation and structural support layers**

```lisp
CivilFoundations
```

**Creates Layers:**
- `C-FOUND-SHALLOW` - Shallow foundation systems
- `C-FOUND-DEEP` - Deep foundation systems
- `C-FOUND-FOOTINGS` - Concrete footings
- `C-FOUND-PILES` - Pile foundations
- `C-FOUND-REBAR` - Reinforcement details
- `C-FOUND-DETAILS` - Foundation construction details

### Layer Categories and Naming Convention

**Naming Pattern:** `C-CATEGORY-ELEMENT`
- **C-** = Civil engineering prefix
- **CATEGORY** = Functional category (DEMO, GRADE, DRAIN, etc.)
- **ELEMENT** = Specific element within category

#### Complete Layer Categories

| Category | Prefix | Description | Color Range |
|----------|--------|-------------|-------------|
| **Base Drawing** | C-BASE-* | Border, grid, notes, dimensions | 7-8 |
| **Existing Conditions** | C-EXIST-* | Topography, buildings, utilities | 8-9 |
| **Survey & Control** | C-SURV-* | Boundaries, setbacks, control points | 30-32 |
| **Site Preparation** | C-DEMO-* | Clearing, demolition, protection | 40-45 |
| **Earthworks & Grading** | C-GRADE-* | Grades, slopes, berms | 50-55 |
| **Excavation & Cuts** | C-EXCAV-* | Excavation limits, shoring | 60-65 |
| **Fill & Embankments** | C-FILL-* | Structural fill, compaction | 70-75 |
| **Foundations** | C-FOUND-* | Shallow/deep foundations | 80-85 |
| **Roads & Access** | C-ROAD-* | Centerlines, pavement | 90-95 |
| **Drainage Systems** | C-DRAIN-* | Storm/sanitary, structures | 100-105 |
| **Utilities** | C-UTIL-* | Water, sewer, gas, electrical | 110-115 |
| **Erosion Control** | C-EROS-* | Temporary/permanent control | 120-125 |
| **Environmental** | C-ENV-* | Wetlands, buffers, protected areas | 130-135 |
| **Construction Staging** | C-STAGE-* | Temporary facilities, permits | 140-145 |

### Integration with FastDraw System

**Automatic Layer Detection:**
When using FastDraw Construction mode, the system automatically detects civil engineering drawing context and switches to appropriate layers:

```lisp
Command: FD
FastDraw Mode: [C]onstruction
Drawing context detected: Grading operations
Auto-switched to layer: C-GRADE-PROP
```

**Pattern Recognition:**
- Grading operations → C-GRADE-* layers
- Foundation work → C-FOUND-* layers  
- Road construction → C-ROAD-* layers
- Drainage design → C-DRAIN-* layers

### Common Workflows

#### Site Development Project
```lisp
; 1. Complete layer setup
CreateCivilLayers
Select: [A]ll layers

; 2. Site preparation phase
; Switch to C-DEMO-CLEAR for site boundaries
; Use C-DEMO-PROTECT for tree protection zones

; 3. Earthwork design
; Use C-GRADE-EXIST for existing contours
; Use C-GRADE-PROP for proposed grading

; 4. Infrastructure
; Use C-ROAD-* for road design
; Use C-DRAIN-* for drainage systems
```

#### Industrial Site Development
```lisp
; 1. Foundation-focused setup
CivilFoundations
CivilEarthwork

; 2. Heavy foundations
; Use C-FOUND-DEEP for pile systems
; Use C-EXCAV-PERM for permanent excavation
; Use C-FOUND-REBAR for reinforcement details
```

### Documentation and Resources

**Complete Documentation:**
- [Civil Engineering Layers Guide](doc/CivilEngineeringLayersGuide.md) - Complete layer reference
- [Civil Engineering Workflows](doc/CivilEngineeringWorkflows.md) - Practical project workflows

**Integration:**
- Compatible with FastDraw v2.0 intelligent drawing system
- Coordinates with Solar and MEP layer systems
- Professional color schemes for clarity and standards compliance

---

## Drawing Management

### Draw Order Commands

#### `BF` - Bring to Front
**Bring selected objects to front of draw order**

```lisp
BF
```

**Usage:**
1. Run command
2. Select objects to bring forward
3. Objects moved to front of draw order

#### `BB` - Send to Back
**Send selected objects to back of draw order**

```lisp
BB
```

#### `BA` - Bring Above
**Bring objects above a reference object**

```lisp
BA
```

**Usage:**
1. Select objects to move
2. Select reference object
3. Selected objects placed above reference

### Drawing Utilities

#### `UnitScale`
**Scale objects with unit conversion and error handling**

```lisp
UnitScale
```

**Features:**
- Multiple selection methods
- Unit conversion capabilities
- Error handling and validation
- Undo support

#### `CreateScale`
**Create professional scale bars**

```lisp
CreateScale
```

### FastDraw v2.0 - AI-Powered Drawing System

#### `FastDraw` / `FD` - Main FastDraw Command
**Advanced AI-powered drawing system with multiple intelligent modes**

```lisp
FastDraw
; or
FD
```

**Drawing Modes:**
- **RAPID** (`FDRapid` / `FDRA`) - Quick drawing with minimal clicks
- **PATTERN** (`FDPattern` / `FDPA`) - Pattern recognition and creation
- **CONSTRUCTION** (`FDConstruction` / `FDCO`) - Construction geometry tools
- **BATCH** (`FDBatch` / `FDB`) - Batch drawing operations
- **PRECISION** (`FDPrecision` / `FDPR`) - Precision drawing tools

**Features:**
- AI-powered drawing assistance
- Context-aware entity creation
- Smart pattern recognition
- Batch operations for repetitive tasks
- Construction geometry automation
- Precision drawing with exact measurements
- Real-time feedback and guidance
- Optimized for GstarCAD performance

#### `FDRapid` / `FDRA` - Rapid Drawing Mode
**Quick drawing with minimal user input**

```lisp
FDRapid
; or
FDRA
```

**Capabilities:**
- Smart entity type detection
- Automatic osnap selection
- Quick dimensioning
- Minimal click drawing
- Context-aware defaults
- Instant entity creation

**Usage:**
1. Run command to enter rapid mode
2. Click points for automatic entity creation
3. System intelligently determines entity types
4. Minimal prompts for maximum speed

#### `FDPattern` / `FDPA` - Pattern Drawing Mode
**Advanced pattern recognition and creation system**

```lisp
FDPattern
; or
FDPA
```

**Pattern Types:**
- **Linear Patterns** - Evenly spaced objects along a line
- **Rectangular Patterns** - Grid-based object arrays
- **Circular Patterns** - Objects arranged in circles or arcs
- **Freeform Patterns** - Custom pattern creation
- **Pattern Analysis** - Detect existing patterns in drawing

**Pattern Operations:**
- **Create Smart Patterns** - AI analyzes selection and creates patterns
- **Linear Array** - Objects along a line with spacing
- **Rectangular Array** - Grid arrays with row/column control
- **Circular Array** - Polar arrays with angle/count control

**Example Usage:**
```lisp
FDPA
; Select: Linear
; Pick base objects, define line and spacing
; System creates intelligent linear pattern
```

#### `FDConstruction` / `FDCO` - Construction Geometry Mode
**Automated construction and reference geometry creation**

```lisp
FDConstruction
; or
FDCO
```

**Construction Tools:**
- **Center Lines** - Automatic centerline creation for objects
- **Axis Lines** - Principal axis line generation
- **Guide Lines** - Reference lines at specific angles/positions
- **Offset Lines** - Parallel lines at specified distances
- **Perpendicular Lines** - Automatic perpendicular construction
- **Construction Layers** - Automatic layer management for construction geometry

**Features:**
- Automatic object analysis for construction needs
- Smart construction layer assignment
- Perpendicular and parallel line automation
- Center point and axis detection
- Reference geometry creation

**Example Usage:**
```lisp
FDCO
; Select: Center Lines
; Pick objects to analyze
; System creates appropriate centerlines automatically
```

#### `FDBatch` / `FDB` - Batch Drawing Mode
**Efficient batch operations for repetitive drawing tasks**

```lisp
FDBatch
; or
FDB
```

**Batch Operations:**
- **Multiple Lines** - Draw multiple lines in sequence
- **Circles at Points** - Create circles at specified point locations
- **Rectangles** - Batch rectangle creation
- **Block Insertion** - Insert multiple blocks efficiently
- **Text Placement** - Batch text creation with consistent formatting

**Features:**
- Continuous operation mode
- Consistent formatting across batch
- Efficient selection and creation
- Automatic spacing and alignment
- Batch property application

**Example Usage:**
```lisp
FDB
; Select: Circles at Points
; Pick multiple points
; System creates circles at all selected points
```

#### `FDPrecision` / `FDPR` - Precision Drawing Mode
**High-precision drawing with exact measurements and coordinates**

```lisp
FDPrecision
; or
FDPR
```

**Precision Tools:**
- **Exact Distance** - Draw with precise distance input
- **Angle Precise** - Exact angle specification
- **Coordinate Entry** - Direct coordinate input for precision
- **Precision Measure** - Exact measurement and verification
- **Grid Align** - Snap to precise grid positions

**Features:**
- Sub-millimeter precision
- Coordinate-based input
- Exact distance and angle control
- Measurement verification
- Grid and snap integration
- Real-time precision feedback

**Example Usage:**
```lisp
FDPR
; Select: Exact Distance
; Pick start point
; Enter exact distance: 1547.25
; System creates line with precise length
```

### FastDraw Helper Functions

**The FastDraw system includes comprehensive helper functions:**

#### Pattern Creation Helpers
- `CreateLinearPattern` - Generate linear patterns
- `CreateRectPattern` - Generate rectangular arrays
- `CreateCircPattern` - Generate circular patterns
- `CreateFreeformPattern` - Generate custom patterns
- `AnalyzePattern` - Detect and analyze existing patterns

#### Array Creation Helpers
- `CreateLinearArray` - Linear object arrays
- `CreateRectArray` - Rectangular grids
- `CreateCircArray` - Circular/polar arrays

#### Construction Geometry Helpers
- `CreateCenterLines` - Automatic centerline generation
- `CreateAxisLines` - Principal axis lines
- `CreateGuideLines` - Reference line creation
- `CreateOffsetLines` - Parallel offset lines
- `CreatePerpLines` - Perpendicular line construction

#### Batch Operation Helpers
- `BatchMultipleLines` - Multiple line creation
- `BatchCirclesAtPoints` - Circles at point locations
- `BatchRectangles` - Multiple rectangle creation
- `BatchBlocks` - Efficient block insertion
- `BatchText` - Batch text creation

#### Precision Drawing Helpers
- `DrawExactDistance` - Precise distance drawing
- `DrawAnglePrecise` - Exact angle drawing
- `CoordinateEntry` - Direct coordinate input
- `PrecisionMeasure` - Exact measurement tools
- `GridAlign` - Grid-based precision alignment

### FastDraw Best Practices

1. **Mode Selection**: Choose the appropriate mode for your task
2. **Pattern Recognition**: Use PATTERN mode for repetitive elements
3. **Construction Geometry**: Use CONSTRUCTION mode for reference lines
4. **Batch Operations**: Use BATCH mode for multiple similar objects
5. **Precision Work**: Use PRECISION mode for exact measurements
6. **Layer Management**: FastDraw automatically manages construction layers
7. **Performance**: System optimized for GstarCAD performance

### Automatic Block Numbering (AutoLabel)

#### `AutoLabel` - Main AutoLabel Command
**Automatic numbering for block attributes with project-specific configurations**

```lisp
AutoLabel
```

**Options:**
- `On` - Enable AutoLabel system
- `Off` - Disable AutoLabel system  
- `Status` - Show current status
- `Config` - Interactive configuration
- `Help` - Show help information
- `Solar` - Apply solar project settings
- `Construction` - Apply construction settings
- `MEP` - Apply MEP project settings

**Features:**
- Automatic attribute numbering for inserted blocks
- Project-specific configurations (Solar, Construction, MEP)
- Wildcard pattern matching for blocks and attributes
- Configurable prefixes, suffixes, and number formatting
- Real-time monitoring with reactors

#### `AutoLabelOn` - Enable AutoLabel
**Enable automatic block attribute numbering**

```lisp
AutoLabelOn
```

#### `AutoLabelOff` - Disable AutoLabel
**Disable automatic block attribute numbering**

```lisp
AutoLabelOff
```

#### `AutoLabelStatus` - Show Status
**Display current AutoLabel configuration and status**

```lisp
AutoLabelStatus
```

**Information Shown:**
- Active/Inactive status
- Current project type
- Starting number and formatting settings
- Block and attribute patterns

#### `AutoLabelConfig` - Interactive Configuration
**Configure AutoLabel settings interactively**

```lisp
AutoLabelConfig
```

**Configuration Options:**
- `Project` - Select project type (Solar/Construction/MEP/General)
- `Numbering` - Configure number formatting (start, increment, length, prefix/suffix)
- `Patterns` - Configure block and attribute patterns
- `Reset` - Reset to default configuration

#### `AutoLabelSolar` - Solar Project Configuration
**Apply optimized settings for solar projects**

```lisp
AutoLabelSolar
```

**Solar Settings:**
- Block patterns: `SOLAR-PANEL*`, `SOLAR-INVERTER*`, `SOLAR-ARRAY*`, `PV-*`
- Attribute patterns: `NUMBER`, `PANEL_ID`, `INVERTER_ID`, `ARRAY_ID`, `TAG`
- 3-digit formatting with zero padding
- Starting from 1

#### `AutoLabelConstruction` - Construction Project Configuration
**Apply optimized settings for construction projects**

```lisp
AutoLabelConstruction
```

**Construction Settings:**
- Block patterns: `*EQUIPMENT*`, `*ROOM*`, `*DOOR*`, `*WINDOW*`
- Attribute patterns: `NUMBER`, `ROOM_NUM`, `EQUIP_TAG`, `ID`
- 3-digit formatting starting from 101

#### `AutoLabelMEP` - MEP Project Configuration
**Apply optimized settings for MEP (Mechanical, Electrical, Plumbing) projects**

```lisp
AutoLabelMEP
```

**MEP Settings:**
- Block patterns: `*PANEL*`, `*HVAC*`, `*PLUMB*`, `*ELECTRICAL*`
- Attribute patterns: `NUMBER`, `PANEL_ID`, `EQUIP_TAG`, `DEVICE_ID`
- 4-digit formatting for complex numbering schemes

#### `AutoLabelHelp` - Show Help
**Display comprehensive help information for AutoLabel system**

```lisp
AutoLabelHelp
```

**Features:**
- Multiple scale formats
- Customizable appearance
- Standard architectural scales
- Automatic text labeling

#### `CreateBeamGrid`
**Create structural beam grids**

```lisp
CreateBeamGrid
```

---

## Navigation & Zoom

### Primary Zoom Commands

#### `ZZ` - Zoom to Objects
**Zoom to selected objects with enhanced options**

```lisp
ZZ
```

**Features:**
- Single or multiple object selection
- Automatic view centering
- Configurable zoom margin
- Error handling

#### `ZV` - Zoom to Extents
**Zoom to show all drawing content**

```lisp
ZV
```

#### `ZB` - Zoom Previous
**Return to previous zoom view**

```lisp
ZB
```

#### `ZW` - Zoom Window
**Define zoom area by window selection**

```lisp
ZW
```

**Enhanced Features:**
- Center point mode
- Dimension-based selection
- Standard window selection

#### `ZWE` - Enhanced Zoom Window
**Zoom window with additional options**

```lisp
ZWE
```

**Options:**
- Center point and dimensions
- Width/height specification
- Enhanced user interface

#### `ZS` - Zoom to Scale
**Zoom to specific scale factor**

```lisp
ZS
```

**Features:**
- Direct scale input
- Standard scale presets
- Real-time preview

#### `ZA` - Zoom All
**Zoom to show all objects**

```lisp
ZA
```

### Layout Management

#### `SL` - Switch Layout
**Switch between drawing layouts**

```lisp
SL
```

**Features:**
- Interactive layout selection
- Layout list display
- Quick switching interface

#### `SLN` - Next Layout
**Switch to next layout in sequence**

```lisp
SLN
```

---

## Layer Management

### Layer Control Commands

#### `LA` - Layer Dialog
**Open the AutoCAD Layer Manager**

```lisp
LA
```

#### `LC` - Make Layer Current
**Make selected object's layer current**

```lisp
LC
```

**Usage:**
1. Run command
2. Select object
3. Object's layer becomes current

#### `LL` - Lock Layer
**Lock layer of selected object**

```lisp
LL
```

#### `LU` - Unlock Layer
**Unlock layer of selected object**

```lisp
LU
```

#### `LF` - Freeze Layer
**Freeze layer of selected object**

```lisp
LF
```

#### `LT` - Thaw All Layers
**Thaw all frozen layers**

```lisp
LT
```

#### `LO` - Turn Layer Off
**Turn off layer of selected object**

```lisp
LO
```

#### `LN` - Turn All Layers On
**Turn on all layers**

```lisp
LN
```

### Advanced Layer Commands

#### `LCH` - Change Layer Properties
**Modify layer color, linetype, etc.**

```lisp
LCH
```

#### `LCR` - Layer Color Change
**Change layer color by selection**

```lisp
LCR
```

### Layer Director (Automatic Layer Management)

#### `LDON` - Enable Layer Director
**Enable automatic layer switching based on commands**

```lisp
LDON
```

**Features:**
- Automatically switches to appropriate layers
- Creates layers with proper properties if needed
- Enhanced for solar and construction workflows

#### `LDOFF` - Disable Layer Director
**Disable automatic layer switching**

```lisp
LDOFF
```

#### `LayerDirectorStatus` - Show Status
**Display Layer Director status and configuration**

```lisp
LayerDirectorStatus
```

**Information Displayed:**
- Current status (Active/Inactive)
- Number of configured commands
- XRef layering status
- Property forcing settings
- LISP command handling

#### `LayerDirectorHelp` - Show Help
**Display comprehensive Layer Director help**

```lisp
LayerDirectorHelp
```

**Automatic Layer Switching Patterns:**

| Command Pattern | Target Layer | Description |
|----------------|--------------|-------------|
| `SOLAR*` | SOLAR-MAIN | Solar system components |
| `*PANEL*` | SOLAR-PANELS | Solar panels |
| `*ARRAY*` | SOLAR-ARRAYS | Solar panel arrays |
| `*INVERTER*` | SOLAR-ELEC | Solar electrical |
| `*STRING*` | SOLAR-ELEC | Solar string wiring |
| `*CONDUIT*` | SOLAR-CONDUIT | Solar conduit runs |
| `*DEMO*` | DEMO | Demolition work |
| `*EXIST*` | EXISTING | Existing conditions |
| `*NEW*` | NEW-WORK | New construction |
| `*ROOF*` | ROOF | Roof elements |
| `*STRUCT*` | STRUCTURE | Structural elements |
| `*ELEC*` | ELECTRICAL | Electrical systems |
| `*HVAC*` | HVAC | HVAC systems |
| `*PLUMB*` | PLUMBING | Plumbing systems |
| `[DM]TEXT,TEXT` | TEXT | Text and labels |
| `DIM*,*LEADER` | DIMENSIONS | Dimensions and leaders |
| `LINE` | LINES | General lines |
| `CIRCLE` | CIRCLES | Circles |
| `HATCH` | HATCH | Hatching and fills |

**Layer Properties:**
- Automatic color assignment
- Appropriate linetypes (Continuous, Hidden, etc.)
- Standard lineweights
- Plot settings
- Layer descriptions

---

## Core Aliases

### Selection Commands

#### `SS` - Select Similar
**Enhanced SelectSimilar with options**

```lisp
SS
```

**Options:**
- All: Select similar in entire drawing
- Current: Select similar in current space

#### `SA` - Select All
**Select all objects in drawing**

```lisp
SA
```

#### `SL` - Select by Layer
**Select objects on same layer**

```lisp
SL
```

#### `SC` - Select by Color
**Select objects with same color**

```lisp
SC
```

### Modify Commands

#### `M` - Enhanced Move
**Move command with enhanced features**

```lisp
M
```

#### `N` - Alternative Move
**Alternative move command interface**

```lisp
N
```

#### `C` - Enhanced Copy
**Copy command with array and multiple options**

```lisp
C
```

**Options:**
- Array: Create rectangular array of copies
- Multiple: Multiple copy placement
- Default: Standard copy operation

### Text Commands

#### `S2` - Advanced Text Scaling
**Scale text with multiple options and calculations**

```lisp
S2
```

**Features:**
- Multiple text selection
- Automatic scaling calculations
- Height analysis and reporting
- Percentage-based scaling

---

## Component Libraries

### Electrical Components

#### `ElecComponent`
**Access electrical component library**

```lisp
ElecComponent
```

**Components:**
- Panels and switchgear
- Outlets and devices
- Lighting fixtures
- Safety equipment

### HVAC Components

#### `HVACEquip`
**Access HVAC equipment library**

```lisp
HVACEquip
```

**Components:**
- Air handling units
- Ductwork fittings
- Diffusers and grilles
- Controls and sensors

### Plumbing Components

#### `PlumbFixture`
**Access plumbing fixture library**

```lisp
PlumbFixture
```

**Components:**
- Fixtures and fittings
- Piping components
- Valves and controls
- Drainage systems

### Fire Protection Components

#### `FireProtection`
**Access fire protection component library**

```lisp
FireProtection
```

**Components:**
- Sprinkler heads
- Fire alarm devices
- Suppression systems
- Safety equipment

### Telecommunications Components

#### `TelecomEquip`
**Access telecommunications equipment library**

```lisp
TelecomEquip
```

**Components:**
- Network equipment
- Communication devices
- Cabling components
- Distribution systems

---

## System Utilities

### Loading and Configuration

#### `LoadLispCADAll`
**Load all LispCAD components with comprehensive checking**

```lisp
LoadLispCADAll
```

**Features:**
- Recursive directory scanning
- Dependency resolution
- Error reporting
- Component registration

#### `ConfigEdit`
**Edit LispCAD configuration interactively**

```lisp
ConfigEdit
```

**Functions:**
- View current settings
- Modify configuration values
- Save changes
- Restore defaults

#### `VerifyLispCAD`
**Verify LispCAD installation and functionality**

```lisp
VerifyLispCAD
```

### Path Management

#### `SetLispCADPath`
**Set base LispCAD path**

```lisp
SetLispCADPath
```

#### `FixLispCADPath`
**Fix path issues automatically**

```lisp
FixLispCADPath
```

#### `ShowSearchPath`
**Display current search paths**

```lisp
ShowSearchPath
```

### Windows Integration

#### `FixWindowsPaths`
**Fix Windows path compatibility issues**

```lisp
FixWindowsPaths
```

#### `CreateWindowsUtils`
**Create Windows-specific utilities**

```lisp
CreateWindowsUtils
```

#### `SetupWindowsLispCAD`
**Complete Windows setup**

```lisp
SetupWindowsLispCAD
```

---

## Testing & Diagnostics

### System Testing

#### `LispCADTest`
**Run comprehensive LispCAD tests**

```lisp
LispCADTest
```

**Test Categories:**
- Core functionality
- Command availability
- File access
- Path resolution

#### `LispCADDiagnostic`
**System diagnostic and health check**

```lisp
LispCADDiagnostic
```

**Diagnostic Areas:**
- AutoCAD environment
- Search paths
- File accessibility
- Component loading

### Solar Testing

#### `SolarTest`
**Run comprehensive solar tools tests**

```lisp
SolarTest
```

#### `SolarBenchmark`
**Performance benchmark for solar tools**

```lisp
SolarBenchmark
```

#### `SolarIntegrationTest`
**Test solar tools integration**

```lisp
SolarIntegrationTest
```

#### `SolarStressTest`
**Stress test solar tools under load**

```lisp
SolarStressTest
```

### Specialized Tests

#### `TestStringpNil`
**Test string processing functions**

```lisp
TestStringpNil
```

#### `TestDrawOrder`
**Test draw order commands**

```lisp
TestDrawOrder
```

#### `CheckFileAccess`
**Test file access capabilities**

```lisp
CheckFileAccess
```

---

## Configuration Management

### Solar Configuration

#### System Status Commands

```lisp
;; Display solar system status
(solar:status)

;; Show detailed help
(solar:help)

;; Force reload all modules
(solar:load-all-modules T)

;; Run comprehensive tests
(solar:test-all)
```

#### Configuration Functions

```lisp
;; Display current configuration
(solar:show-config)

;; Set configuration value
(solar:set-config 'CATEGORY 'KEY value)

;; Save configuration to file
(solar:save-config "path/to/config.lsp")

;; Load configuration from file
(solar:load-config "path/to/config.lsp")

;; Reset to defaults
(solar:reset-config)
```

### Global Configuration

```lisp
;; Edit global configuration
ConfigEdit

;; Set base path
SetLispCADPath

;; Show current paths
ShowSearchPath
```

---

## Command Index

### Alphabetical Command List

| Command | Description | Category |
|---------|-------------|----------|
| `BA` | Bring objects above reference | Drawing |
| `BB` | Send objects to back | Drawing |
| `BF` | Bring objects to front | Drawing |
| `C` | Enhanced copy with options | Core |
| `CheckFileAccess` | Test file access | Testing |
| `ConfigEdit` | Edit configuration | System |
| `CreateBeamGrid` | Create beam grid | Drawing |
| `CreateScale` | Create scale bar | Drawing |
| `CreateWindowsUtils` | Windows utilities | System |
| `ElecComponent` | Electrical components | Library |
| `FastDraw` | AI-powered drawing system | Drawing |
| `FD` | FastDraw (alias) | Drawing |
| `FDB` | FastDraw Batch mode | Drawing |
| `FDBatch` | FastDraw Batch mode | Drawing |
| `FDCO` | FastDraw Construction mode | Drawing |
| `FDConstruction` | FastDraw Construction mode | Drawing |
| `FDPA` | FastDraw Pattern mode | Drawing |
| `FDPattern` | FastDraw Pattern mode | Drawing |
| `FDPR` | FastDraw Precision mode | Drawing |
| `FDPrecision` | FastDraw Precision mode | Drawing |
| `FDRA` | FastDraw Rapid mode | Drawing |
| `FDRapid` | FastDraw Rapid mode | Drawing |
| `FixLispCADPath` | Fix path issues | System |
| `FixWindowsPaths` | Fix Windows paths | System |
| `FireProtection` | Fire protection components | Library |
| `GCR` | Ground Coverage Ratio (alias) | Solar |
| `GroundCoverageRatio` | GCR calculator (alias) | Solar |
| `HVACEquip` | HVAC components | Library |
| `LA` | Layer dialog | Layer |
| `LayerDirectorHelp` | Layer Director help | Layer Director |
| `LayerDirectorStatus` | Layer Director status | Layer Director |
| `LC` | Make layer current | Layer |
| `LDOFF` | Disable Layer Director | Layer Director |
| `LDON` | Enable Layer Director | Layer Director |
| `LF` | Freeze layer | Layer |
| `LL` | Lock layer | Layer |
| `LN` | Turn all layers on | Layer |
| `LO` | Turn layer off | Layer |
| `LT` | Thaw all layers | Layer |
| `LU` | Unlock layer | Layer |
| `LispCADDiagnostic` | System diagnostic | Testing |
| `LispCADTest` | System tests | Testing |
| `LoadLispCADAll` | Load all components | System |
| `LoadSolarTools` | Load solar tools | Solar |
| `M` | Enhanced move | Core |
| `N` | Alternative move | Core |
| `OptimizeArray` | Array optimization | Solar |
| `PlumbFixture` | Plumbing components | Library |
| `ReloadSolarTools` | Reload solar tools | Solar |
| `S2` | Advanced text scaling | Text |
| `SA` | Select all | Selection |
| `SC` | Select by color | Selection |
| `SetLispCADPath` | Set base path | System |
| `SetupWindowsLispCAD` | Windows setup | System |
| `ShowSearchPath` | Show paths | System |
| `SL` | Switch layout / Select by layer | Navigation/Selection |
| `SLN` | Next layout | Navigation |
| `SolarArray` | Solar array layout | Solar |
| `SolarBenchmark` | Solar performance test | Testing |
| `SolarConfig` | Solar configuration | Solar |
| `SolarDocs` | Generate documentation | Solar |
| `SolarGCR` | GCR calculator | Solar |
| `SolarInfoBlock` | Solar info block | Solar |
| `SolarIntegrationTest` | Solar integration test | Testing |
| `SolarRadiation` | Solar radiation analysis | Solar |
| `SolarRegistry` | Solar component registry | Solar |
| `SolarSetback` | Solar setback calculator | Solar |
| `SolarStressTest` | Solar stress test | Testing |
| `SolarStrings` | Solar string layout | Solar |
| `SolarTest` | Solar system tests | Testing |
| `SolarTools` | Main solar menu | Solar |
| `SS` | Select similar | Selection |
| `SunPath` | Sun path analysis | Solar |
| `TelecomEquip` | Telecom components | Library |
| `TestDrawOrder` | Test draw order | Testing |
| `TestStringpNil` | Test string functions | Testing |
| `UnitScale` | Scale with units | Drawing |
| `VerifyLispCAD` | Verify installation | System |
| `ZA` | Zoom all | Navigation |
| `ZB` | Zoom previous | Navigation |
| `ZS` | Zoom to scale | Navigation |
| `ZV` | Zoom extents | Navigation |
| `ZW` | Zoom window | Navigation |
| `ZWE` | Enhanced zoom window | Navigation |
| `ZZ` | Zoom to objects | Navigation |

---

## Usage Examples

### Basic Solar Workflow

```lisp
;; 1. Load solar tools
(load-solar-cad-tools)

;; 2. Check system status
(solar:status)

;; 3. Run GCR calculation
SolarGCR
;; Follow prompts for panel specs, array config, ground area

;; 4. Create array layout
SolarArray
;; Configure and place array in drawing

;; 5. Add information block
SolarInfoBlock
```

### Drawing Management Workflow

```lisp
;; 1. Set up layers
CreateSolarConstructionLayers

;; 2. Manage draw order
BF    ; Bring critical elements forward
BB    ; Send backgrounds to back

;; 3. Use FastDraw for efficient drawing
FD    ; Main FastDraw system
FDRA  ; Rapid drawing mode
FDPA  ; Pattern creation mode
FDCO  ; Construction geometry
FDB   ; Batch operations
FDPR  ; Precision drawing

;; 4. Add scale and annotations
CreateScale
SolarInfoBlock

;; 5. Navigate efficiently
ZZ    ; Zoom to specific objects
ZV    ; View entire drawing
```

### FastDraw Workflow Examples

```lisp
;; Pattern Creation Workflow
FDPA
;; Select: Linear Pattern
;; Pick base objects, define spacing
;; System creates intelligent pattern

;; Construction Geometry Workflow  
FDCO
;; Select: Center Lines
;; Pick objects to analyze
;; System creates centerlines automatically

;; Batch Drawing Workflow
FDB
;; Select: Circles at Points
;; Pick multiple points
;; System creates circles at all points

;; Precision Drawing Workflow
FDPR
;; Select: Exact Distance
;; Pick start point, enter precise distance
;; System creates line with exact length
```

### Component Library Usage

```lisp
;; Access component libraries
ElecComponent      ; Electrical
HVACEquip         ; HVAC
PlumbFixture      ; Plumbing
FireProtection    ; Fire protection
TelecomEquip      ; Telecommunications
```

### System Maintenance

```lisp
;; Regular maintenance
LispCADDiagnostic  ; Check system health
LispCADTest        ; Run functionality tests
ConfigEdit         ; Adjust settings

;; Troubleshooting
FixLispCADPath     ; Fix path issues
VerifyLispCAD      ; Verify installation
ShowSearchPath     ; Check paths
```

---

## Best Practices

### Performance Optimization

1. **Load Only Required Modules**: Use specific loaders for targeted functionality
2. **Regular Testing**: Run diagnostic commands periodically
3. **Path Management**: Keep paths properly configured
4. **Configuration Backup**: Save working configurations

### Solar Project Workflow

1. **Planning Phase**: Use `SolarTools` menu for comprehensive planning
2. **GCR Analysis**: Always run `SolarGCR` before finalizing layouts
3. **Documentation**: Generate documentation with `SolarDocs`
4. **Validation**: Use testing commands to verify results

### Error Handling

1. **Check Dependencies**: Verify required modules are loaded
2. **Path Issues**: Use path fixing utilities when needed
3. **Testing**: Run relevant test commands after significant changes
4. **Backup**: Maintain backups of working configurations

---

## Support and Troubleshooting

### Common Issues

**Solar Tools Not Loading**
```lisp
;; Check status
(solar:status)

;; Force reload
(solar:load-all-modules T)

;; Run diagnostics
(solar:test-all)
```

**Path Problems**
```lisp
;; Check paths
ShowSearchPath

;; Fix automatically
FixLispCADPath

;; Set manually
SetLispCADPath
```

**General Issues**
```lisp
;; Run diagnostics
LispCADDiagnostic

;; Test functionality
LispCADTest

;; Check file access
CheckFileAccess
```

### Getting Help

- Use `(solar:help)` for solar tools help
- Run diagnostic commands for system information
- Check configuration with `ConfigEdit`
- Review documentation in `doc/` folder

---

**Document Version:** 1.0  
**Last Updated:** June 8, 2025  
**Author:** LispCAD Development Team

For technical support and updates, refer to the project documentation in the `doc/` directory.
