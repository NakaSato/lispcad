# File Naming Improvement Guide for LispCAD

This document provides a guide for improving the file naming convention in the LispCAD project.

## Current Naming Issues
- Inconsistent naming patterns (camelCase, prefixed 'c', ALL_CAPS, descriptive_names)
- Lack of category indication in filenames
- Difficulty identifying file purpose from name alone

## New Naming Convention
We recommend adopting the format: `LC_Category_Function.lsp` where:
- `LC` is a prefix for all LispCAD files (stands for LispCAD)
- `Category` indicates the module/category (Core, Drawing, etc.)
- `Function` describes the specific functionality

## Recommended File Renaming

### Core Commands
| Current Filename | Recommended Filename |
|------------------|----------------------|
| ALIASEDIT.lsp | LC_Core_Aliases.lsp |
| cCLIPIT.lsp | LC_Core_ClipboardText.lsp |

### Drawing Management
| Current Filename | Recommended Filename |
|------------------|----------------------|
| BringObjects.lsp | LC_Drawing_DrawOrder.lsp |
| CreateBeamGrid.lsp | LC_Drawing_BeamGrid.lsp |
| CreateScale.lsp | LC_Drawing_ScaleBar.lsp |
| UnitScale.lsp | LC_Drawing_UnitConverter.lsp |

### Navigation Utilities
| Current Filename | Recommended Filename |
|------------------|----------------------|
| ZoomCommands.lsp | LC_Navigation_ZoomTools.lsp |
| cSwitchLayout.lsp | LC_Navigation_LayoutSwitcher.lsp |

### Publishing Tools
| Current Filename | Recommended Filename |
|------------------|----------------------|
| cPUBPAPER.lsp | LC_Publish_PaperLayouts.lsp |

### Document Maintenance
| Current Filename | Recommended Filename |
|------------------|----------------------|
| AutoPurgeOnSave.lsp | LC_Document_AutoPurge.lsp |
| cAutoPurge.lsp | LC_Document_PurgeTools.lsp |
| MASTER_EREF.lisp | LC_Document_XRefManager.lsp |

### Advanced Objects
| Current Filename | Recommended Filename |
|------------------|----------------------|
| Flex_Duct_Centerline.lsp | LC_Advanced_FlexDuct.lsp |

### System Utilities
| Current Filename | Recommended Filename |
|------------------|----------------------|
| LispCAD_Utils.lsp | LC_Utils_Core.lsp |

## Implementation Steps

1. Create a copy of each file with the new name
2. Update file headers to reference the previous name
3. Update any documentation references (README.md, README_TH.md)
4. Update any cross-references between files
5. Test that all commands still function correctly
6. Once verified, remove the old files

## Benefits of New Naming Convention

- Easier identification of file purpose
- Consistent sorting in file explorers (all LispCAD files grouped together)
- Clear indication of which category a file belongs to
- Improved maintainability for future developers
- Better organization for documentation

## Note

This renaming should be done gradually to ensure no functionality is broken. Always test thoroughly after each batch of files is renamed.
