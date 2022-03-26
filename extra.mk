ifeq ($(OS),Windows_NT)
	CMP  := fc.exe
	CP   := copy
	KILL := taskkill /im x64sc 2>NUL
	SEP  := \\
else
    CMP  := cmp --verbose
	CP   := cp
	KILL := killall x64sc
	SEP  := /
endif

ifeq ($(words $(TARGETLIST)),1)

# "Last Known Good"
CHECKPOINT_DIR := lkg

# Backup a copy of the current .prg/.lbl files to CHECKPOINT_DIR
checkpoint: $(PROGRAM)
	@-$(MKDIR) $(CHECKPOINT_DIR)
	$(CP) $(PROGRAM) $(CHECKPOINT_DIR)$(SEP)$(PROGRAM)
	$(CP) $(PROGRAM).lbl $(CHECKPOINT_DIR)$(SEP)$(PROGRAM).lbl

# Compare the current .prg/.lbl with the last saved checkpoint.  Helpful for
# verifying that changes have no unintended effects on the emitted binary.
compare: $(PROGRAM)
	$(CMP) "$(CHECKPOINT_DIR)$(SEP)$(PROGRAM)" "$(PROGRAM)"
	$(CMP) "$(CHECKPOINT_DIR)$(SEP)$(PROGRAM).lbl" "$(PROGRAM).lbl"

# Launch program under VICE with scripted debugging commands.
debug: $(PROGRAM)
	@-$(KILL)
	$(EMUCMD) -moncommands "debug.cmds" "$(PROGRAM)"

# Kill all instances of VICE
kill: $(PROGRAM)
	@-$(KILL)

else # $(words $(TARGETLIST)),1

checkpoint compare debug kill:
	$(foreach t,$(TARGETLIST),$(MAKE) TARGETS=$t $@$(NEWLINE))

endif # $(words $(TARGETLIST)),1
