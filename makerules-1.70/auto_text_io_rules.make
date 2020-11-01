# rules for creating Text_IO child packages

%-text_io.ads : %.ads
	auto_text_io.exe -t -f $(AUTO_TEXT_IO_OPTS) $< $(AUTO_TEXT_IO_DIR)

%-gen_text_io.ads : %.ads
	auto_text_io.exe -t -f $(AUTO_TEXT_IO_OPTS) $< $(AUTO_TEXT_IO_DIR)

# end of file
