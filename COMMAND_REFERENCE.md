# LispCAD Command Reference

**Version:** 3.0  
**Date:** June 8, 2025  
**Comprehensive guide to all available commands and functionality**

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Solar Project Tools](#solar-project-tools)
3. [Drawing Management](#drawing-management)
4. [Navigation & Zoom](#navigation--zoom)
5. [Layer Management](#layer-management)
6. [Core Aliases](#core-aliases)
7. [Component Libraries](#component-libraries)
8. [System Utilities](#system-utilities)
9. [Testing & Diagnostics](#testing--diagnostics)
10. [Configuration Management](#configuration-management)
11. [Command Index](#command-index)

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

;; 3. Add scale and annotations
CreateScale
SolarInfoBlock

;; 4. Navigate efficiently
ZZ    ; Zoom to specific objects
ZV    ; View entire drawing
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
