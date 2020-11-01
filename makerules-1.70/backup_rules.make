# Rules for using Gnu tar to make backups

# We force a single -C, so all files in the tar.gz file are uniquely
# named. Be sure to backup the Makefile and the .snar file. Note that
# the log file has directories listed first, then files; consider
# sorting it.
backup-incr :
	tar $(BACKUP_TAR_OPTIONS)czf $(BACKUP_FILENAME).tar.gz \
	--listed-incremental=$(BACKUP_FILENAME).snar \
	--exclude-from=$(BACKUP_EXCLUDE_PATTERN_FILE) \
	-C $(BACKUP_ROOT) $(BACKUP_FILES) > $(BACKUP_FILENAME).log

backup-full :
	rm -f $(BACKUP_FILENAME).snar
	$(MAKE) backup-incr

# end of file
