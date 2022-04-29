import argparse
import sys
import re
import os
from os.path import exists, abspath
import shutil

# IsabelleDSL Master Script

class isabelleDSL:

    file_extensions = {
        "python":".py",
        "c":".c"
    }

    exec_commands = {
        "python":"python3 ",
        "c":"gcc FILE -O file.out; ./file.out"
    }

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
        parser.add_argument("-b", "--boilerplate_file", help="File with boilerplate code to insert sessions into.")
        parser.add_argument("-f", "--pp_func", help="Pretty-printer function to use.")
        parser.add_argument("--auto_test", action="store_true", help="Run tests to automatically discern program correctness based on sessions.")
        parser.add_argument("--test_string", help="String to insert user sessions into for testing. Sessions will be inserted in between -- -- string.")
        parser.add_argument("--session_type", help="If session is polymorphic, e.g. a list, provide a type for user_session strings during auto testing.")

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

    def find_funcs(self):
        """find all function names in file"""

        func_pat = re.compile(r'fun (\w+)')
        self.theory_funcs = re.findall(func_pat, self.theory_file)

    def create_temp_theory(self):
        """create a temporary theory file with additional export_code commands"""

        if not self.args.theory_file:
            pass
        else:
            #find all functions so they can be exported
            self.find_funcs()

            #add code to theory file to create intermediary Haskell code
            self.temp_theory_file = self.theory_file + "\n\n"

            #copy original code

            with open(self.args.pp_func) as f:
                self.pp_func_code = f.read()

            #create string of list of functions

            pp_func_string = self.pp_func_code + "\n\n"

            funcs = " ".join(self.theory_funcs)

            #generate strings for haskell-equivalent user sessions
            
            session_defs = ""
            
            if self.args.session_type is None:
                session_signature = "session"
            else:
                session_signature = self.args.session_type + " session"

            self.usess_defs = []

            #create definitions in the style 'definition test :: "int session" where mytest [code] :'
            #when exported to haskell this will give us user_session strings in correct syntax
            for i in range(0, len(self.user_sessions)):
                session_defs += "\ndefinition usess" + str(i) + " :: \"" + session_signature + "\" where mysess" + str(i) + "[code] :"
                session_defs += "\n\"usess" + str(i) + " = " + self.user_sessions[i] + "\""
                self.usess_defs.append("usess" + str(i))

            session_defs += "\n\n"

            export_code_string = "export_code pp " + funcs + " " + " ".join(self.usess_defs) + " in Haskell module_name " + self.args.module_name + " file_prefix " + self.args.module_name.lower()

            new_code = pp_func_string + session_defs + export_code_string + "\n\nend"

            self.temp_theory_file = re.sub(r'^end$', new_code, self.temp_theory_file, flags=re.MULTILINE)

            #write to temporary theory file
            self.thy_filepath = "/tmp/" + self.thy_name + '.thy'
            with open(self.thy_filepath, 'w') as f:
                f.write(self.temp_theory_file)

            if exists(self.orig_path + "/Resources/StringUtils.thy"):
                shutil.copy(self.orig_path + "/Resources/StringUtils.thy", "/tmp/StringUtils.thy")
            else:
                print("Resources/StringUtils.thy file not found. Check this has not been deleted. Exiting.")
                exit()

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

        if self.args.verbose:
            print("Modified theory file created at " + self.thy_filepath)

    def run_temp_theory(self):
        #build the theory
        print("\nBuilding theory file...")
        os.chdir('/tmp')
        
        #store result of build command
        if self.args.verbose:
            res = os.popen('isabelle export -d . -x "*:**.hs" ' + self.thy_name).read()
            print("Build results:\n\n----\n" + res + "----\n")
        else:
            res = os.popen('isabelle export -d . -x "*:**.hs" ' + self.thy_name).read()

        #check for errors
        if "FAILED" in res:
            print("Build failed. Try running Isabelle theory file manually, or running with --verbose flag to inspect errors.")
            exit()
        else:
            print("Done building\n")

        self.hs_file = '/tmp/export/' + self.args.module_name + '.' + self.args.module_name + '/code/' + self.args.module_name.lower() + '/' + self.args.module_name + '.hs'

        with open(self.hs_file) as f:
            hs_code = f.read()

        #regexes for datatype definitions and imports
        datatype_re = r'(newtype|data) ((.|\n  \|)*);'
        import_re = r'import Prelude \(((.|\n)*)\);\nimport qualified Prelude;'

        #add deriving Show statements to all datatypes
        hs_code = re.sub(datatype_re, r'\1 \2\n  deriving Show;', hs_code, flags=re.MULTILINE)

        #import Prelude.Show
        hs_code = re.sub(import_re, r'import Prelude (\1, Show);', hs_code, flags=re.MULTILINE)

        #create the haskell file
        with open(self.hs_file, 'w') as f:
            f.write(hs_code)

        #run the haskell file
        if self.args.verbose:
            print("Running Haskell file (" + self.hs_file + ")")
        else:
            print("Running Haskell file")
        
        self.sessions_strings = []

        #run haskell file, passing each session to the pp function
        for s in self.user_sessions:
            res = os.popen('ghci ' + self.hs_file + ' -e "pp (' + s + ')"').read()
            self.sessions_strings.append(res[1:(len(res) - 2)])

        #reset working directory
        os.chdir(self.orig_path)

    def insert_boilerplate(self):
        """insert pretty-printed sessions code into user-defined boilerplate"""

        print("Inserting pretty-printed sessions into boilerplate code")

        #get current boilerplate code
        with open(self.args.boilerplate_file) as f:
            self.boilerplate_text = f.read()

        #find the SESSIONS[] string in the boilerplate and extract the sessions placeholder contents inside it
        sess_re = r'SESSIONS\[((.|\n)*)\]'
        sessions_placeholder = re.search(sess_re, self.boilerplate_text, flags=re.MULTILINE).group(1)

        sessions_string = ""

        #format all pretty-printed sessions strings and add them to string
        for sess in self.sessions_strings:
            sessions_string += (sessions_placeholder.replace('[session]', sess) + "\n")

        #insert sessions into the SESSIONS[] string
        new_text = re.sub(sess_re, sessions_string, self.boilerplate_text, flags=re.MULTILINE)

        #write the final exported file
        self.export_file_path = self.args.output_directory + "/export" + self.file_extensions[self.args.target_language]
        with open(self.export_file_path, 'w') as f:
            f.write(new_text)

    def test_export(self):
        """run the exported file with each test case"""
        print("\n=== List of User Sessions (Haskell Syntax) ===\n")
        for s in self.user_sessions:
            print(s)

        #evaluate haskell file with each user session, inserted into the placeholder command
        print("\n=== Test Cases from Exported Haskell Code ===\n")
        for s in self.user_sessions:
            test_string = self.args.test_string.replace("----", s)
            cmd = 'ghci ' + self.hs_file + ' -e "' + test_string + '"'
            res = os.popen(cmd).read()
            print(res)

        #run the exported file using the syntax for the target language so user can compare results
        if self.args.verbose:
            print("\n=== Results of Exported File (" + self.export_file_path + ") ===\n")
        else:
            print("\n=== Results of Exported File ===\n")
        cmd = self.exec_commands[self.args.target_language] + self.export_file_path
        res = os.popen(cmd).read()
        print(res)

    def main(self):
        self.parse_args()

        self.orig_path = os.getcwd()

        #check required parameters supplied

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

        if not self.args.boilerplate_file:
            self.args.boilerplate_file = input("File with boilerplate code must be supplied! Enter filepath:\n")

            if len(self.args.boilerplate_file) < 1:
                exit()

        if not self.args.pp_func:
            self.args.pp_func = input("File with pretty-print function code must be supplied! Enter filepath:\n")

            if len(self.args.pp_func) < 1:
                exit()

        print("\n=== Generating DSL ===\n")

        #read user-defined sessions
        with open(self.args.sessions_file) as f:
            self.user_sessions = f.read().splitlines()

        #get theory file name and path
        if self.args.theory_file:
            self.tf_dir, self.tf_name = os.path.split(abspath(self.args.theory_file))

            self.thy_name = self.tf_name.split(".thy")[0]

        self.args.target_language = self.args.target_language.lower()

        #default values for parameters
        if not self.args.module_name:
            self.args.module_name = self.thy_name

        if not self.args.output_directory:
            self.args.output_directory = self.orig_path

        #load the theory file or project folder
        self.load_theory()

        #create a temporary theory file
        self.create_temp_theory()

        #run the file to trigger the export_code process
        self.run_temp_theory()

        #insert the exported code into provided boilerplate + export to disk
        self.insert_boilerplate()

        #run testing functions if required
        if self.args.auto_test:
            if not self.args.test_string:
                self.args.test_string = input("Input string to be formatted with test cases (user sessions). User sessions inserted between -- -- (e.g. \"eval (----)\"):\n")

                if len(self.args.test_string) < 1:
                    exit()

            self.test_export()

if __name__ == '__main__':
    idsl = isabelleDSL()
    idsl.main()