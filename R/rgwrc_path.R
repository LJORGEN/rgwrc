#Create a directory to be used as the link between miniconda and R
#dir.create("C:/Users/Public/rgwrc_hold")
dir.create(file.path(system.file(package="rgwrc"), "rgwrc_hold"))
#python imports
#ospy <- import("os")
#get the current working directories
#ospy$getcwd() #"C:\\Users\\ljorg\\OneDrive\\Documents" #default cwd
#set the cwd to the rgwrc_hold, which contains the python scripts
#ospy$chdir(file.path(system.file(package="rgwrc"), "rgwrc_hold"))
#ospy$chdir("C:/Users/ljorg/OneDrive/Documents")

