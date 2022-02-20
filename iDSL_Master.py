import argparse
import sys
import re

# IsabelleDSL Master Script

class isabelleDSL:
    def parse_args(self):
        #setup argparse
        parser = argparse.ArgumentParser(prog="iDSL_Master.py", description="Convert an Isabelle project to a domain-specific language")
        
        #optional flags
        #must have one of -t (for a .thy file) or -p (for a project folder)
        parser.add_argument("-t", "--theory_file", help="Load a theory file.")
        parser.add_argument("-p", "--project_folder", help="Load a project from a folder.")
        parser.add_argument("-l", "--target_language", help="Host language to output DSL in.")
        parser.add_argument("-o", "--output_directory", help="Directory to output files to. Default is CWD.")
        parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode.")

        #parse arguments
        self.args = parser.parse_args()

    def load_theory(self):
        """load a theory file, create a temporary file with export_code command
        run temporary file to export intermediary code"""

        with open(self.args.theory_file, 'r') as f:
            self.theory_file = f.read()

        # print(self.theory_file)

        self.find_funcs()

    def find_funcs(self):
        """find function definitions in theory file"""

        func_pat = re.compile(r'fun (\w+)')

        self.theory_funcs = re.findall(func_pat, self.theory_file)
        # self.theory_funcs = re.search(func_pat, self.theory_file)

        print(self.theory_funcs)

    def main(self):
        self.parse_args()

        if not self.args.theory_file and not self.args.project_folder:
            print("One of -t or -p must be supplied!")
            exit()

        if not self.args.target_language:
            print("Target language must be supplied!")
            exit()

        self.load_theory()

if __name__ == '__main__':
    idsl = isabelleDSL()
    idsl.main()