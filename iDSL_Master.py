import argparse
import sys
import re
import os
from os.path import exists, abspath
import shutil

# IsabelleDSL Master Script

class isabelleDSL:

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
        parser.add_argument("-s", "--sessions_file", help="File with user sessions, separated by newlines.")

        #parse arguments
        self.args = parser.parse_args()

    def load_theory(self):
        """load a theory file and extract its custom functions"""

        if not self.args.theory_file:
            #TODO: how to handle a project folder?
            pass
        else:
            with open(self.args.theory_file, 'r') as f:
                self.theory_file = f.read()

    def create_root_file(self):
        """create a root file
        TODO: can we use export_files here?"""
        pass

    def create_temp_theory(self):
        """create a temporary theory file with additional export_code commands"""

        if not self.args.theory_file:
            pass
        else:
            #add code to theory file to create intermediary Haskell code
            self.temp_theory_file = self.theory_file + "\n\n"

            self.temp_theory_file = re.sub(r'^end$', "\n\nexport_code pp in Haskell module_name " + self.args.module_name + " file_prefix " + self.args.module_name.lower() + "\n\nend", self.temp_theory_file, flags=re.MULTILINE)

            #write to temporary theory file
            with open("/tmp/" + self.thy_name + '.thy', 'w') as f:
                f.write(self.temp_theory_file)

            #TODO: check if ROOT file contains an export-files command, if not exit/print warning
            if exists(self.tf_dir + "/ROOT"):
                print("Existing ROOT file found - copying - please make sure this file contains an export_files statement")
                shutil.copy(self.tf_dir + "/ROOT", "/tmp/ROOT")
            else:
                #if no existing ROOT file, create one from template
                with open('./Resources/ROOT.template') as f:
                    root_contents = f.read().replace('TheoryName', self.thy_name)

                with open("/tmp/ROOT", 'w') as f:
                    f.write(root_contents)

    def run_temp_theory(self):
        #build the theory
        print("Building theory file...")
        os.chdir('/tmp')
        os.system('isabelle export -d . -x "*:**.hs" ' + self.thy_name)

        print("Done building")

        print("Adding main function to Haskell file")

        #create a main function that calls pp on each user session
        with open('/tmp/export/' + self.args.module_name + '.' + self.args.module_name + '/code/' + self.args.module_name.lower() + '/' + self.args.module_name + '.hs', 'r') as f:
            text = f.read()

        newtext = "\n\nmain :: IO ()\nmain =\n  do\n"

        for s in self.user_sessions:
            newtext += "\n    pp (" + s + ")"

        newtext += "\n\n}"

        text = re.sub(r'^}$', newtext, text, flags=re.MULTILINE)

        with open('/tmp/export/' + self.args.module_name + '.' + self.args.module_name + '/code/' + self.args.module_name.lower() + '/' + self.args.module_name + '.hs', 'w') as f:
            f.write(text)

        #run the haskell file
        print("Running Haskell file")
        os.system('runghc /tmp/export/' + self.args.module_name + '.' + self.args.module_name + '/code/' + self.args.module_name.lower() + '/' + self.args.module_name + '.hs')

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

        if not self.args.sessions_file:
            self.args.sessions_file = input("File with user sessions (sequence of actions to perform) must be supplied! Enter filepath:\n")

            if len(self.args.sessions_file) < 1:
                exit()

        with open(self.args.sessions_file) as f:
            self.user_sessions = f.read().splitlines()

        if self.args.theory_file:
            self.tf_dir, self.tf_name = os.path.split(abspath(self.args.theory_file))

            self.thy_name = self.tf_name.split(".thy")[0]

        self.args.target_language = self.args.target_language.lower()

        #default values for parameters
        if not self.args.module_name:
            self.args.module_name = self.thy_name

        if not self.args.output_directory:
            self.args.output_directory = os.getcwd()

        #load the theory file or project folder
        self.load_theory()

        #create a temporary theory file
        self.create_temp_theory()

        self.run_temp_theory()

if __name__ == '__main__':
    idsl = isabelleDSL()
    idsl.main()