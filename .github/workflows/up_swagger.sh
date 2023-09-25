#!/bin/bash

set -eux

# Get Swagger UI version and compare to imported one. Exit if same...
npm install swagger-ui-dist
rm -f package-lock.json
rm -f package.json
NEW_SWAGGER_VSN=$(jq -r .version <node_modules/swagger-ui-dist/package.json)
OLD_SWAGGER_VSN=$(cat SWAGGER_VSN)

if [[ "${NEW_SWAGGER_VSN}" = "${OLD_SWAGGER_VSN}" ]]; then
    # no change
    exit
fi

# Swagger UI version is different from imported one. Update...
rm -rf priv/swagger
mkdir -p priv/swagger
mv -f node_modules/swagger-ui-dist/* priv/swagger
rm -rf node_modules

# Same as https://github.com/swagger-api/swagger-ui/blob/63ad6f6a5bce19075e717ea74acaf9f7055dcdf5/docker/docker-entrypoint.d/40-swagger-ui.sh#L12
FIND="\"https://petstore.swagger.io/v2/swagger.json\""
REPLACE="window.location.origin + \"/api-docs/swagger.json\""
sed -i -e "s|${FIND}|${REPLACE}|g" priv/swagger/swagger-initializer.js

echo "${NEW_SWAGGER_VSN}" >SWAGGER_VSN

git config user.name "GitHub Actions"
git config user.email "actions@user.noreply.github.com"

BRANCH=feature/swagger-ui-update

if git show-ref --verify --quiet "refs/heads/${BRANCH}"; then
    # already exists
    exit
fi

git fetch origin
git checkout -b "${BRANCH}"

if ! git diff --exit-code 1>/dev/null; then
    # there's stuff to push
    git add .
    git commit -m "Update Swagger UI to ${NEW_SWAGGER_VSN}"
    git push origin "${BRANCH}"

    gh pr create --fill \
        --title "Update Swagger UI to ${NEW_SWAGGER_VSN} (automation)" \
        --body "This is an automated action to update the repository's Swagger UI version"
fi
