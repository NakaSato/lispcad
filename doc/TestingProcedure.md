# LispCAD Testing Procedure

This document provides step-by-step instructions for manually testing the fixes for the "bad argument type: stringp nil" error in LispCAD.

## Preparation

1. Ensure all LispCAD files are in the correct directory structure.
2. Start AutoCAD with a new drawing.

## Test 1: Basic Loading Test

This test verifies that the LispCAD commands load properly.

1. Run the `APPLOAD` command in AutoCAD.
2. Navigate to the LispCAD directory and select `LispCAD_Loader.lsp`.
3. Click "Load".
4. Verify in the command line that it shows "Loading LispCAD commands..." and other loading messages without errors.

**Expected Result**: Commands should load without errors or warnings.

## Test 2: Draw Order Commands

This test verifies that the draw order commands work after the fixes.

1. Draw a few objects (lines, circles, etc.) that overlap each other.
2. Run the `BF` command.
3. Select one or more objects to bring to the front.
4. Run the `BB` command.
5. Select one or more objects to send to the back.
6. Run the `BA` command.
7. Select one or more objects to reorder.
8. When prompted, select a reference object to place the first selection above.

**Expected Result**: Objects should reorder as expected without any errors.

## Test 3: Standalone File Testing

This test verifies that the draw order commands work when loaded individually, without using the main loader.

1. Start a new AutoCAD session.
2. Run the `APPLOAD` command.
3. Navigate to `/src/drawing/LC_Drawing_DrawOrder.lsp` and load it directly.
4. Try the `BF`, `BB`, and `BA` commands as in Test 2.

**Expected Result**: Commands should work without any "stringp nil" or other errors.

## Test 4: Automated Tests

This test runs the automated test suite to verify the fixes.

1. Start a new AutoCAD session.
2. Run the `APPLOAD` command.
3. Navigate to the LispCAD directory and select `LispCAD_Loader.lsp`.
4. Click "Load".
5. Run the `APPLOAD` command again.
6. Navigate to `/src/utils/LispCAD_Test.lsp` and load it.
7. Run the `LispCADTest` command to run all automated tests.
8. Run the `TestStringpNil` command to specifically test for the "stringp nil" error.
9. Run the `TestDrawOrder` command to test the draw order commands.

**Expected Result**: All tests should pass with no errors.

## Test 5: Edge Case Testing

These tests verify specific edge cases that might cause the "stringp nil" error.

1. Create a new folder structure with only the essential files copied to a different location:
   - LispCAD_Loader.lsp
   - src/utils/LispCAD_Utils.lsp
   - src/drawing/LC_Drawing_DrawOrder.lsp
2. Open AutoCAD and load LC_Drawing_DrawOrder.lsp directly from this new location.
3. Try running the `BF` command.

**Expected Result**: Command should work without errors even when loaded from a non-standard location.

## Troubleshooting

If you encounter any issues during testing:

1. Run the `TestUtilsLoading` command to diagnose utility loading issues.
2. Check the AutoCAD command line for any error messages.
3. Verify that all files are in the correct location according to the directory structure.
4. Try restarting AutoCAD and loading the files again.

## Reporting Results

After completing these tests, please document:

1. Which tests passed and which failed
2. Any error messages encountered
3. The AutoCAD version you used for testing
4. Any other observations about the behavior of the commands

This will help ensure the fixes are working as expected across different environments and identify any remaining issues.
