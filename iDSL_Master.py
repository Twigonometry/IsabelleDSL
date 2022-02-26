import argparse
import sys
import re

# IsabelleDSL Master Script

class isabelleDSL:
    
    export_string = 'ML {{*\nval gen_files = Generated_Files.get_files (Proof_Context.theory_of @{{context}})\nval output_dir = Path.explode \"{0}\"\n*}}\n\nML {{* map (Generated_Files.write_file output_dir) gen_files *}}'

    def parse_args(self):
        #setup argparse
        parser = argparse.ArgumentParser(prog="iDSL_Master.py", description="Convert an Isabelle project to a domain-specific language")
        
        #optional flags
        #must have one of -t (for a .thy file) or -p (for a project folder)
        parser.add_argument("-t", "--theory_file", help="Load a theory file.")
        parser.add_argument("-p", "--project_folder", help="Load a project from a folder.")
        parser.add_argument("-m", "--module_name", help="The name of the module to export to.")
        parser.add_argument("-l", "--target_language", help="Host language to output DSL in.")
        parser.add_argument("-o", "--output_directory", help="Directory to output files to. Default is CWD.")
        parser.add_argument("-v", "--verbose", action="store_true", help="Verbose mode.")

        #parse arguments
        self.args = parser.parse_args()

    def load_theory(self):
        """load a theory file and extract its custom functions"""

        with open(self.args.theory_file, 'r') as f:
            self.theory_file = f.read()

        self.find_funcs()

    def find_funcs(self):
        """find function definitions in theory file"""

        func_pat = re.compile(r'fun (\w+)')

        self.theory_funcs = re.findall(func_pat, self.theory_file)

    def create_temp_theory(self):
        """create a temporary theory file with additional export_code commands"""

        functions_string = " ".join(self.theory_funcs)

        #add code to theory file to create intermediary Haskell code
        self.temp_theory_file = self.theory_file + "\n\nexport_code " + functions_string + " in Haskell module_name" + self.args.module_name + " file_prefix " + self.args.module_name.lower()

        self.temp_theory_file = self.temp_theory_file + "\n\n" + self.export_string.format(self.args.output_directory)

        print(self.temp_theory_file)

    def main(self):
        self.parse_args()

        if not self.args.theory_file and not self.args.project_folder:
            print("One of -t or -p must be supplied!")
            exit()

        if not self.args.target_language:
            self.args.target_language = input("Target language must be supplied! Enter language:\n")

            #TODO: add a check here for allowed export languages
            if len(self.args.target_language) < 1:
                exit()

        #default values for parameters
        if not self.args.module_name:
            self.args.module_name = "Export"

        if not self.args.output_directory:
            self.args.output_directory = "./"

        #load the theory file or project folder
        self.load_theory()

        #create a temporary theory file
        self.create_temp_theory()

if __name__ == '__main__':
    idsl = isabelleDSL()
    idsl.main()