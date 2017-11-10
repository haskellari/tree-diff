#!/usr/bin/env python3

import getopt
import json
import os
import subprocess
import sys
import tempfile

def usage():
    print("./cabal-new-install.py pkg-name exe-name install-dir")

def main():
    # Parser arguments
    try:
        opts, args = getopt.getopt(sys.argv[1:], "", [])
    except getopt.GetoptError as err:
        # print help information and exit:
        print(err) # will print something like "option -a not recognized"
        usage()
        sys.exit(2)

    if len(args) != 3:
        usage()
        sys.exit(2)

    pkgName, exeName, installDir =  args
    installDir = os.path.abspath(installDir)

    # Check if file exists, if so - do nothing
    installedExe = os.path.join(installDir, exeName)
    if os.path.isfile(installedExe):
        print("Executable seems to be installed: {}".format(installedExe))
        sys.exit(0)

    # Make a temporary directory to work in
    tmpDir = tempfile.mkdtemp()
    os.chdir(tmpDir)

    template = """
    name:                dummypkg
    version:             0
    build-type:          Simple
    cabal-version:       >=2.0

    library
      default-language: Haskell2010
      build-tool-depends: {}:{}
    """

    # Write .cabal file
    with open("dummypkg.cabal", "w") as out:
        out.write(template.format(pkgName, exeName))

    # Build pkg
    subprocess.check_call(["cabal", "new-build", "lib:dummypkg"])

    # Find a location of bin
    with open("dist-newstyle/cache/plan.json") as f:
        j = json.load(f);

        done = False
        for dep in j["install-plan"]:
            if dep["pkg-name"] == pkgName and dep["component-name"] == "exe:" + exeName:
                binFile = dep["bin-file"]

                # Making a symbolic link
                subprocess.check_call(["cp", "-svf", binFile, installDir])

                # We are done
                done = True
                break

        if not done:
            print("Cannot find executable {}:exe:{}".format(pkgName, exeName))
            sys.exit(3)

if __name__ == "__main__":
    main()
