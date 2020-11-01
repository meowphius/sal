# common parts of makefiles for SAL

.PHONY : tests times

tests :: test_all_harness.diff
tests :: test-config_files.diff
tests :: test-config_files-time.diff
tests :: test_gen_alg_find_binary_integer_arrays.diff
tests :: test_gen_alg_find_binary_puppet_arrays.diff
tests :: test_gen_alg_find_binary_symbol_arrays.diff
tests :: test_gen_alg_find_linear_integer_lists.diff
tests :: test_gen_alg_find_linear_puppet_lists.diff
tests :: test_gen_alg_find_linear_symbol_lists.diff
tests :: test_gen_alg_find_linear_integer_lists_sorted.diff
tests :: test_gen_alg_find_linear_puppet_lists_sorted.diff
tests :: test_gen_alg_find_linear_symbol_lists_sorted.diff
tests :: test_gen_array_image.diff
tests :: test_gen_fifo.diff
tests :: test_gen_images.diff
tests :: test_gen_lists_double.diff
tests :: test_gen_stacks_bounded.diff
tests :: test_gen_word_order_convert.diff
tests :: test_gray_code.diff
tests :: test_memory_streams_address.diff
tests :: test_memory_streams_bounded.diff
tests :: test_poly_alg_find_linear.diff
tests :: test_poly_binary_trees_sorted.diff
tests :: test_poly_function_tables_monotonic_first_order.diff
tests :: test_poly_lists_double.diff
tests :: test_poly_lists_single.diff
tests :: test_poly_stacks_unbounded_array.diff
tests :: test_poly_unbounded_arrays.diff
tests :: test_simple_function_tables_monotonic_first_order.diff

tests :: test_gen_math_gen_dof_3_gen_image.diff
tests :: test_gen_math_gen_square_array.diff
tests :: test_gen_math_gen_vector.diff
tests :: test_math_double_dof_2.diff
tests :: test_math_double_dof_6.diff
tests :: test_math_double_scalar.diff
tests :: test_math_float_dof_2.diff
tests :: test_math_float_dof_6_integrator_utils.diff
tests :: test_math_float_dof_6.diff
tests :: test_math_float_inverse_array.diff
tests :: test_math_float_scalar.diff

# Make sure these compile
tests :: debug_math_double_runge_kutta_4th.exe
tests :: debug_math_float_gen_runge_kutta_4th.exe
tests :: debug_math_float_manipulator_6.exe
tests :: debug_math_float_manipulator_7.exe

times : time_inverse_inertia.out
times : time_math_float_den_hart.out
times : time_poly_function_tables_monotonic_first_order.out
times : time_simple_function_tables_monotonic_first_order.out

test-config_files.diff : test-config_files.good_config test-config_files.config
	diff -u -w $^ > $@

test-config_files-time.diff : test-config_files-time.good_config test-config_files-time.config
	diff -u -w $^ > $@

# We don't delete the *text_io packages here, because we don't
# generate them here; that's done in the Auto_Text_IO makefiles.
clean ::
	rm -f *.config

# end of file
