import argparse
import sys

# IsabelleDSL Master Script

class isabelleDSL:
    def parse_args(self):
        #setup argparse
        parser = argparse.ArgumentParser(prog="iDSL_Master.py", description="Convert an Isabelle project to a domain-specific language")
        
        #optional flags
        #must have one of -t (for a .thy file) or -p (for a project folder)
        parser.add_argument("-t", "--theory_file", help="Load a theory file.")
        parser.add_argument("-p", "--project_folder", help="Load a project from a folder.")
        parser.add_argument("-l", "--target_language")
        parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode.")

        #parse arguments
        self.args = parser.parse_args()

    def main(self):
        self.parse_args()

        if not self.args.theory_file and not self.args.project_folder:
            print("One of -t or -p must be supplied!")
            exit()

if __name__ == '__main__':
    idsl = isabelleDSL()
    idsl.main()