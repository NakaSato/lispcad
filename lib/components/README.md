# LispCAD Component Libraries Documentation

## Overview

The LispCAD Component Libraries system provides a comprehensive framework for managing and using standardized building components across electrical, plumbing, HVAC, and mechanical disciplines. This documentation covers usage, extension, and best practices.

## System Components

### Core Framework Files
- **ComponentFramework.lsp** - Core framework for component management
- **LibraryManager.lsp** - Tools for creating and managing libraries
- **LibraryLoader.lsp** - Automated library loading system

### Component Libraries
- **ElectricalComponents.lsp** - Electrical components and calculations
- **PlumbingComponents.lsp** - Plumbing fixtures and pipe sizing
- **HVACComponents.lsp** - HVAC equipment and ductwork
- **MechanicalComponents.lsp** - Pumps, fans, motors, and compressors

## Getting Started

### Loading Libraries
```lisp
;; Load all component libraries
(command "LoadLibraries")

;; Check loading status
(command "LibraryStatus")

;; Verify library integrity
(command "VerifyLibraries")
```

### Using Component Libraries

#### Electrical Components
```lisp
;; Access electrical component
(command "ElecComponent")

;; Calculate conduit fill
(elec:conduit-fill-calc "1/2" "EMT" 5)

;; Draw conduit run
(command "DrawConduit")
```

#### Plumbing Components
```lisp
;; Insert plumbing fixture
(command "PlumbFixture")

;; Calculate pipe flow
(plumb:hazen-williams-flow 4.0 1000.0 150.0)

;; Size drainage pipe
(command "SizeDrain")
```

#### HVAC Components
```lisp
;; Select HVAC equipment
(command "HVACEquip")

;; Size ductwork
(hvac:size-rectangular-duct 2000 800)

;; Draw duct run
(command "DrawDuct")
```

#### Mechanical Components
```lisp
;; Select mechanical equipment
(command "MechEquip")

;; Calculate pump power
(mech:pump-power-calc 100 50 0.75)
```

## Component Data Structure

All components follow a standardized data format:

```lisp
(list "component-name" '(
  (type . "component-type")
  (description . "component description")
  (specifications . ((spec1 . value1) (spec2 . value2)))
  (properties . ((prop1 . value1) (prop2 . value2)))
  (dimensions . ((width . value) (height . value) (depth . value)))
  (performance . ((capacity . value) (efficiency . value)))
))
```

## Creating New Libraries

### Using Library Manager
```lisp
;; Create new library template
(command "NewLibrary")

;; Validate library structure
(command "ValidateLibrary")

;; Create backup
(command "BackupLibrary")

;; Get library information
(command "LibraryInfo")
```

### Manual Library Creation

1. **Create Library File Structure**
```lisp
;; MyComponents.lsp
;; Custom Component Library
;; Author: Your Name
;; Version: 1.0

(defun MY:init ()
  "Initialize custom component library"
  (princ "\nLoading My Components Library...")
  (setq *MY:COMPONENTS* nil)
  T
)
```

2. **Add Component Data**
```lisp
(setq MY:SAMPLE-COMPONENT
  (list "my-component" '(
    (type . "custom")
    (description . "Custom component description")
    (specifications . ((size . "standard") (material . "steel")))
    (properties . ((weight . 10.0) (cost . 100.0)))
  ))
)
```

3. **Add Access Functions**
```lisp
(defun MY:get-component (component-name)
  "Retrieve component by name"
  ;; Implementation here
)

(defun MY:draw-component (component-name insertion-point)
  "Draw component at specified point"
  ;; Implementation here
)
```

4. **Add Command Interface**
```lisp
(defun c:MyComponent ()
  "Custom component selection command"
  ;; Interactive component selection
  (princ)
)
```

## Extending Existing Libraries

### Adding Components to Existing Libraries

```lisp
;; Add component using Library Manager
(lm:add-component-to-library 
  "ElectricalComponents.lsp" 
  "new-device"
  '((type . "device") (description . "New electrical device"))
)
```

### Component Validation

All components should be validated before adding:

```lisp
;; Validate component data
(cf:validate-component-data 
  component-data 
  '(name type description specifications)
)

;; Validate numeric ranges
(cf:validate-numeric-range value 0 100 "Efficiency")
```

## Best Practices

### Component Definition
1. **Use Consistent Naming** - Follow library naming conventions
2. **Include Complete Specifications** - Provide all relevant technical data
3. **Add Performance Data** - Include capacity, efficiency, or flow rates
4. **Specify Dimensions** - Include physical dimensions for drawing
5. **Include Cost Information** - Add cost data when available

### Library Organization
1. **Group Related Components** - Organize by function or type
2. **Use Meaningful Names** - Component names should be descriptive
3. **Include Documentation** - Add comments explaining complex data
4. **Version Control** - Track library versions and changes
5. **Test Thoroughly** - Validate all functions before deployment

### Performance Optimization
1. **Load Libraries Selectively** - Only load needed libraries
2. **Cache Frequently Used Data** - Store common components in memory
3. **Use Efficient Data Structures** - Optimize for fast lookup
4. **Minimize File I/O** - Batch file operations when possible

## Calculation Functions

### Electrical Calculations
```lisp
;; Conduit fill calculation
(elec:conduit-fill-calc conduit-size conduit-type wire-count)

;; Load calculation
(elec:electrical-load-calc connected-load demand-factor)

;; Voltage drop calculation
(elec:voltage-drop-calc current distance wire-size voltage)
```

### Plumbing Calculations
```lisp
;; Flow capacity using Hazen-Williams
(plumb:hazen-williams-flow diameter length c-factor)

;; DFU to GPM conversion
(plumb:dfu-to-gpm dfu-value)

;; Pipe sizing for drainage
(plumb:size-drain-pipe dfu-value slope)
```

### HVAC Calculations
```lisp
;; Duct sizing
(hvac:size-rectangular-duct cfm velocity)

;; Round duct equivalent
(hvac:equivalent-round-duct width height)

;; Equipment selection
(hvac:select-equipment load-type capacity)
```

### Mechanical Calculations
```lisp
;; Pump power calculation
(mech:pump-power-calc flow-rate head efficiency)

;; Fan power calculation  
(mech:fan-power-calc cfm static-pressure efficiency)
```

## Drawing Functions

All libraries include drawing utilities:

### Block Insertion
```lisp
;; Insert component block
(component:insert-block block-name insertion-point scale rotation)

;; Create component symbol
(component:create-symbol symbol-data layer-name)
```

### Line Drawing
```lisp
;; Draw pipe/conduit/duct runs
(component:draw-run start-point end-point line-type layer)

;; Add connection points
(component:add-connections points connection-type)
```

## Integration with LispCAD

### Layer Management
- Components automatically use appropriate layers
- Layer properties defined in component data
- Integration with existing layer standards

### Block Management
- Automatic block creation and insertion
- Block attribute handling
- Integration with existing block libraries

### Calculation Integration
- Results can be used with other LispCAD tools
- Data export capabilities
- Integration with project databases

## Troubleshooting

### Common Issues

1. **Library Not Loading**
   - Check file path and permissions
   - Verify file syntax with ValidateLibrary
   - Check for missing dependencies

2. **Component Not Found**
   - Verify component name spelling
   - Check if library is loaded
   - Use SearchComponents to find components

3. **Calculation Errors**
   - Validate input parameters
   - Check for numeric range violations
   - Review calculation function parameters

### Error Handling

Libraries include comprehensive error handling:
- Input validation
- Range checking
- File operation error handling
- User-friendly error messages

## Support and Updates

### Getting Help
- Use ComponentFramework command for system information
- Check LibraryStatus for loading issues
- Use SearchComponents to find specific components

### Updating Libraries
- Create backups before updates
- Validate libraries after changes
- Test all functions after updates
- Document changes in library headers

## Future Enhancements

Planned improvements include:
- Additional component types
- Enhanced calculation functions
- Better integration with CAD standards
- Expanded drawing capabilities
- Web-based component browser
