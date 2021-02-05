function swprof;
    if test -z "$argv"
	set -gx GUIX_PROFILE $GUIX_DEFAULT_PROFILE
    else
	set -gx GUIX_PROFILE $GUIX_EXTRA_PROFILES/$argv/$argv
    end
    bass source $GUIX_PROFILE/etc/profile
    echo "switched to profile $argv"
end
