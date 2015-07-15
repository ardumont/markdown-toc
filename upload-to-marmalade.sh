#!/usr/bin/env bash
# depends on curl

uploadFile () {
    local username=$1
    local tokenFile=$(cat $2)
    local packageFile=$3
    local marmaladeUrl=${4:-"https://marmalade-repo.org"}
    curl --insecure \
         -F "name=${username}" \
         -F "token=$tokenFile" \
         -F "package=@${packageFile};filename=$(basename ${packageFile})" \
         $marmaladeUrl/v1/packages
}

uploadFile ardumont ~/.marmalade/token ./markdown-toc.el
