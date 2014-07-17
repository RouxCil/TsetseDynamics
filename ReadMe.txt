========================================================================
    Modgen 11 Time-based model : TsetseDynamics Project Overview
========================================================================

The Modgen 11 Time-based model wizard has created this TsetseDynamics application for you.  
This file contains a summary of what you will find in each of the files that
make up your TsetseDynamics application, as well as instructions on what to do next.


TsetseDynamics.vcxproj
    This is the main project file for VC++ projects.  The settings for the project were 
    modified specifically for Modgen 11 models.  Although they can be modified, it is recommended
    that only experienced developer do so.


Files included in the project have been group under 3 filters:

- C++ Files
- Modules (mpp)
- Scenarios

Modules (mpp)

    Mpp Files are the modules of the model and contain the actual Modgen code.  Each mpp file 
    constitues a module of the model.  Run Modgen 11 through the "Run Modgen 11" toolbar button
    or item of the menu "Tools" to generate corresponding C++ files before compiling.

C++ FILES
 
    The C++ files that were created by the wizard are placeholders.  Compiling the project
    at this stage will result in errors.  Model developers need to run Modgen 11 through the
    "Run Modgen 11" toolbar button or item of the menu "Tools" to generate real C++ code before
    compiling the model.

SCENARIOS

    Scenario files are included so that the model developer has a basic scenario to start 
    using the model once it is compiled.  Although they have been included in the C++ project,
    they are not used to compile the model.  Therefore, model developers can choose to remove
    those files from the C++ project.

/////////////////////////////////////////////////////////////////////////////
The Modgen 11 Time-based model wizard has created the following modules:

TsetseDynamics.mpp

    This module contains core simulation functions and definitions.

TsetseFlyCore.mpp

    This module contains the basic information which defines the TsetseFly case.

TickerCore.mpp

	This module contains the basic information associated with a Ticker actor.
	One single Ticker actor is instantiated in a simulation.  It is used to synchronise
	time among all of the actors in the population in order to ensure
	accurate reports.

ModelStrings.mpp

	This module contains model-specific strings used in the progress dialog.

/////////////////////////////////////////////////////////////////////////////
The Modgen 11 Time-based model wizard has created the following C++ files:

TsetseDynamics.cpp
TsetseFlyCore.cpp
ModelStrings.cpp
TickerCore.cpp

    These cpp files are placeholders and do not currently contain valid cpp code.  Running 
    Modgen 11 will replace these files by C++ files corresponding to the mpp modules.

Actors.h
Actors.cpp
model.h
model.rc
parse.inf
Tabinit.h
Tabinit.cpp

    These files are also placeholders and the current content is not valid.  Running Modgen 11
    will replace these files by special files C++ files corresponding to the current model.

app.ico

    This file contains the default Modgen model icon.  It is recommended that model developers
    replace this file with their own icon representative of the model.

/////////////////////////////////////////////////////////////////////////////
The Modgen 11 Time-based model wizard has created the following scenario files:

Base.sce

   This file contains the settings of the basic scenario included with the model.  To view and
   edit this file, it is recommended that model developers first compile their model and 
   then use the model's interface.  To open the scenario in the model, use the "Scenario / Open" 
   menu in the model executable.  Once the scenario is opened, settings can be viewed and
   modified using the "Scenario / Settings" menu item.  Refer to the Modgen 11 User's Guide 
   for more information.

Base(TsetseFlyCore).dat

   This file contains values for the parameters declared in the TsetseFlyCore module.
   These values can be viewed and modified through the Visual Studio environment, or through
   the model interface once the model is compiled.  To see and edit parameter values in the
   model interface, open the scenario and double-click on the parameter in the left pane.


/////////////////////////////////////////////////////////////////////////////
What to do next

- Run Modgen 11: either through the "Run Modgen 11" toolbar button or through the corresponding
  menu item

- Build the model: use the "Build" command or "Batch Build" to create both debug and release
  versions of the model

- run the simulation: open the model executable.  Open the scenario using the "Scenario / Open"
  menu item.  Run the simulation using the "Scenario / Run/resume" menu item or corresponding toolbar
  button.

Or

- edit the model: developers can modify existing modules or add new ones.  To add a new module,
  developers are encouraged to use the new Modgen 11 module wizard which is available through
  the "Add new item" dialog in Visual Studio 2010.

/////////////////////////////////////////////////////////////////////////////
