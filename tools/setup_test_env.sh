#!/bin/zsh
# LispCAD Test Environment Setup
# Creates test environments for verifying loading mechanisms

# Set base directories
SOURCE_DIR="/Users/chanthawat/Library/CloudStorage/OneDrive-Personal/My Files/CAD/lispcad"
TEST_DIR="/Users/chanthawat/Desktop/LispCAD_Test"

# Command line options
while getopts ":c:h" opt; do
  case $opt in
    c)
      # Create specified test case
      case $OPTARG in
        1)
          TEST_CASE="basic"
          ;;
        2)
          TEST_CASE="standalone"
          ;;
        3)
          TEST_CASE="minimal"
          ;;
        4)
          TEST_CASE="mixed"
          ;;
        *)
          echo "Invalid test case: $OPTARG"
          exit 1
          ;;
      esac
      ;;
    h)
      echo "Usage: $0 [-c test_case] [-h]"
      echo "  -c: Create test environment for specific test case"
      echo "      1: Basic test environment (full structure)"
      echo "      2: Standalone files (no main loader)"
      echo "      3: Minimal files (only essential files)"
      echo "      4: Mixed structure (files in non-standard locations)"
      echo "  -h: Show this help message"
      exit 0
      ;;
    \?)
      echo "Invalid option: -$OPTARG"
      exit 1
      ;;
    :)
      echo "Option -$OPTARG requires an argument."
      exit 1
      ;;
  esac
done

# Create test directory if it doesn't exist
if [[ ! -d "$TEST_DIR" ]]; then
  mkdir -p "$TEST_DIR"
  echo "Created test directory: $TEST_DIR"
fi

# Function to create test environment
create_test_env() {
  local env_name=$1
  local env_dir="$TEST_DIR/$env_name"
  
  echo "Creating $env_name test environment..."
  
  # Create directory structure
  mkdir -p "$env_dir"
  
  case $env_name in
    "basic")
      # Full directory structure for basic test
      mkdir -p "$env_dir/src/utils" "$env_dir/src/drawing" "$env_dir/src/core"
      
      # Copy files
      cp "$SOURCE_DIR/LispCAD_Loader.lsp" "$env_dir/"
      cp "$SOURCE_DIR/src/utils/LispCAD_Utils.lsp" "$env_dir/src/utils/"
      cp "$SOURCE_DIR/src/utils/LispCAD_Test.lsp" "$env_dir/src/utils/"
      cp "$SOURCE_DIR/src/drawing/LC_Drawing_DrawOrder.lsp" "$env_dir/src/drawing/"
      
      echo "Created basic test environment at $env_dir"
      echo "To test: Load LispCAD_Loader.lsp from this location in AutoCAD"
      ;;
      
    "standalone")
      # Standalone files without loader
      mkdir -p "$env_dir/drawing"
      
      # Copy only necessary files, not the loader
      cp "$SOURCE_DIR/src/utils/LispCAD_Utils.lsp" "$env_dir/"
      cp "$SOURCE_DIR/src/drawing/LC_Drawing_DrawOrder.lsp" "$env_dir/drawing/"
      
      echo "Created standalone test environment at $env_dir"
      echo "To test: Load drawing/LC_Drawing_DrawOrder.lsp directly in AutoCAD"
      ;;
      
    "minimal")
      # Just the essential files in a flat structure
      cp "$SOURCE_DIR/src/drawing/LC_Drawing_DrawOrder.lsp" "$env_dir/"
      
      echo "Created minimal test environment at $env_dir"
      echo "To test: Load LC_Drawing_DrawOrder.lsp directly in AutoCAD"
      echo "This tests the most challenging case - should still work with local loader"
      ;;
      
    "mixed")
      # Mixed structure with files in unusual locations
      mkdir -p "$env_dir/lisp/utils" "$env_dir/commands"
      
      # Copy files to non-standard locations
      cp "$SOURCE_DIR/LispCAD_Loader.lsp" "$env_dir/"
      cp "$SOURCE_DIR/src/utils/LispCAD_Utils.lsp" "$env_dir/lisp/utils/"
      cp "$SOURCE_DIR/src/drawing/LC_Drawing_DrawOrder.lsp" "$env_dir/commands/"
      
      # Create a simple loader that points to the non-standard locations
      cat > "$env_dir/CustomLoader.lsp" << EOL
;;; ===== CUSTOM LOADER FOR TESTING =====
;;; This loader uses non-standard paths to test utility loading robustness

(defun load-utils ()
  (load (findfile "lisp/utils/LispCAD_Utils.lsp"))
)

(load (findfile "commands/LC_Drawing_DrawOrder.lsp"))

(princ "\\nCustom loader using non-standard paths")
(princ)
EOL
      
      echo "Created mixed test environment at $env_dir"
      echo "To test: Load CustomLoader.lsp from this location in AutoCAD"
      ;;
  esac
}

# Create test environments based on command line options
if [[ -n "$TEST_CASE" ]]; then
  case $TEST_CASE in
    "basic")
      create_test_env "basic"
      ;;
    "standalone")
      create_test_env "standalone"
      ;;
    "minimal")
      create_test_env "minimal"
      ;;
    "mixed")
      create_test_env "mixed"
      ;;
  esac
else
  # Create all test environments if no specific one is requested
  create_test_env "basic"
  create_test_env "standalone"
  create_test_env "minimal"
  create_test_env "mixed"
fi

echo ""
echo "Test environments created successfully in $TEST_DIR"
echo "Use these environments to verify the LispCAD utility loading mechanism"
echo "Follow the testing procedure in doc/TestingProcedure.md"
