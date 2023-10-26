#! /usr/bin/awk -f 
# this restores submodules from a .gitmodules file
# useful for zip downloads of repos
/\[submodule/ {
    gsub("\\]", "", $2)
    name = $2
}
/path =/ {
    path = $3
}
/url =/ {
    url = $3
}
name && path && url {
    printf "Adding submodule: %s\n", name
    system("rm -rf " path)
    system("git submodule add " url " " path)
    name = ""
    path = ""
    url = ""
}
