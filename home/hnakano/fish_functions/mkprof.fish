function mkprof;
    set profile_name $argv[1]
    set rest_args $argv[2..-1]
    set profile_dir $GUIX_EXTRA_PROFILES/$profile_name/$profile_name
    if test -e $profile_dir
	mkdir -p $profile_dir
    end
    command guix package -p $profile_dir $rest_args
end
