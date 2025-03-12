import os
import sys


def concatenate_files(directory_path, output_file="concatenated_output.txt"):
    # Check if directory exists
    if not os.path.isdir(directory_path):
        print(f"Error: {directory_path} is not a valid directory")
        sys.exit(1)

    try:
        # Get all files in the directory
        files = os.listdir(directory_path)
        files.sort()

        # Open output file for writing
        with open(output_file, "w", encoding="utf-8") as outfile:
            for filename in files:
                file_path = os.path.join(directory_path, filename)

                # Skip directories, only process files
                if os.path.isfile(file_path):
                    # Write the filename header
                    outfile.write(f"{filename}:\n")

                    # Try to read and write the file contents
                    try:
                        with open(file_path, "r", encoding="utf-8") as infile:
                            contents = infile.read()
                            outfile.write(contents)
                    except Exception as e:
                        outfile.write(f"Error reading file: {str(e)}\n")

                    # Add a newline after each file
                    outfile.write("\n")

        print(f"All files have been concatenated into {output_file}")

    except Exception as e:
        print(f"An error occurred: {str(e)}")
        sys.exit(1)


if __name__ == "__main__":
    # Check if directory path is provided as command line argument
    if len(sys.argv) < 2:
        print("Usage: python script.py <directory_path> [output_file]")
        sys.exit(1)

    directory_path = sys.argv[1]

    # Optional output file name
    output_file = "concatenated_output.txt"
    if len(sys.argv) > 2:
        output_file = sys.argv[2]

    concatenate_files(directory_path, output_file)
