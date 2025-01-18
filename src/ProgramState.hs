module ProgramState where

import Path (Path)
import FileSystemStructures (Directory)

type ProgramState = (Path, String, Directory)

type ProgramStateError = ProgramState

-- exports ProgramState, ProgramStateError