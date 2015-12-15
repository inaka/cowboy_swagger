# Change Log

## [0.1.0-otp17.](https://github.com/inaka/cowboy-swagger/tree/0.1.0-otp17.) (2015-12-15)
[Full Changelog](https://github.com/inaka/cowboy-swagger/compare/0.1.0...0.1.0-otp17.)

**Closed issues:**

- Location in response headers is not reading path values [\#43](https://github.com/inaka/cowboy-swagger/issues/43)

## [0.1.0](https://github.com/inaka/cowboy-swagger/tree/0.1.0) (2015-12-03)
[Full Changelog](https://github.com/inaka/cowboy-swagger/compare/0.0.1...0.1.0)

**Fixed bugs:**

- /api-docs not working as expected [\#38](https://github.com/inaka/cowboy-swagger/issues/38)
- The catch all path '...' added by cowboy-swagger introduces some confusion when expecting 404 [\#28](https://github.com/inaka/cowboy-swagger/issues/28)
- Fix validate\_metadata spec [\#27](https://github.com/inaka/cowboy-swagger/issues/27)
- static\_files as an env variable is not good for releases [\#26](https://github.com/inaka/cowboy-swagger/issues/26)
- swagger UI always sends JSON [\#24](https://github.com/inaka/cowboy-swagger/issues/24)
- Trails added by swagger itself should be ignored in swagger.json [\#23](https://github.com/inaka/cowboy-swagger/issues/23)
- basePath is misinterpreted [\#22](https://github.com/inaka/cowboy-swagger/pull/22) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Closed issues:**

- Version Bump to 0.1.0 [\#37](https://github.com/inaka/cowboy-swagger/issues/37)
- Allow more than one server running on the same node [\#34](https://github.com/inaka/cowboy-swagger/issues/34)
- Add Meta Testing [\#30](https://github.com/inaka/cowboy-swagger/issues/30)
- basePath should be considered part of the trails path [\#25](https://github.com/inaka/cowboy-swagger/issues/25)

**Merged pull requests:**

- \[Fix \#37\] Bump version to 0.1.0 [\#42](https://github.com/inaka/cowboy-swagger/pull/42) ([harenson](https://github.com/harenson))
- \[Fix \#38\] Add redirect for /api-docs ... [\#41](https://github.com/inaka/cowboy-swagger/pull/41) ([harenson](https://github.com/harenson))
- \[\#25\] Add basePath to swaggerSpec [\#40](https://github.com/inaka/cowboy-swagger/pull/40) ([harenson](https://github.com/harenson))
- \[Fix \#34\] Allow more than one server running on the same node [\#39](https://github.com/inaka/cowboy-swagger/pull/39) ([harenson](https://github.com/harenson))
- \[Fix \#28\] Change catch-all path [\#36](https://github.com/inaka/cowboy-swagger/pull/36) ([harenson](https://github.com/harenson))
- \[Fix \#26\] Fix static\_files trail [\#35](https://github.com/inaka/cowboy-swagger/pull/35) ([harenson](https://github.com/harenson))
- Add meta testing [\#32](https://github.com/inaka/cowboy-swagger/pull/32) ([harenson](https://github.com/harenson))
- \[fix \#27\] Fix metadata spec [\#31](https://github.com/inaka/cowboy-swagger/pull/31) ([harenson](https://github.com/harenson))
- Ferigis.23.ignore swagger trails [\#29](https://github.com/inaka/cowboy-swagger/pull/29) ([ferigis](https://github.com/ferigis))

## [0.0.1](https://github.com/inaka/cowboy-swagger/tree/0.0.1) (2015-09-14)
**Closed issues:**

- Validate and/or set default values of `cowboy\_swagger:metadata\(\)`, according with swagger schema. [\#9](https://github.com/inaka/cowboy-swagger/issues/9)
- Implement static handler to serve the .html contained in `priv/swagger` folder [\#6](https://github.com/inaka/cowboy-swagger/issues/6)
- Implement `trails\_handler:trails/0` callback in Cowboy swagger handler module \(swagger\_handler\). [\#5](https://github.com/inaka/cowboy-swagger/issues/5)
- Implement `cowboy\_swagger:to\_json/1` function [\#4](https://github.com/inaka/cowboy-swagger/issues/4)
- Implement GET method in 'swagger\_handler' in order to retrieve the JSON specification \(swagger.json\) [\#3](https://github.com/inaka/cowboy-swagger/issues/3)
- Setup Swagger-UI into the project. [\#2](https://github.com/inaka/cowboy-swagger/issues/2)
- Fulfil the open-source check-list [\#1](https://github.com/inaka/cowboy-swagger/issues/1)

**Merged pull requests:**

- \[\#1\] Fixed documentation in modules. Fixed README. [\#21](https://github.com/inaka/cowboy-swagger/pull/21) ([cabol](https://github.com/cabol))
- Make link text and href point at the same URL [\#18](https://github.com/inaka/cowboy-swagger/pull/18) ([lavrin](https://github.com/lavrin))
- \[\#1\] fulfil open source list: README. [\#17](https://github.com/inaka/cowboy-swagger/pull/17) ([cabol](https://github.com/cabol))
- \[\#1\] Fulfil the open-source check-list: implemented example. [\#15](https://github.com/inaka/cowboy-swagger/pull/15) ([cabol](https://github.com/cabol))
-  \[\#9\] Validate mandatory fields in the metadata. [\#14](https://github.com/inaka/cowboy-swagger/pull/14) ([cabol](https://github.com/cabol))
- Fixed cowboy-swagger to be a lib \(removed app and sup modules\). Added… [\#12](https://github.com/inaka/cowboy-swagger/pull/12) ([cabol](https://github.com/cabol))
- Cabol.3.cowboy swagger handler [\#10](https://github.com/inaka/cowboy-swagger/pull/10) ([cabol](https://github.com/cabol))
- Implemented cowboy\_swagger:to\_json/1 function. [\#8](https://github.com/inaka/cowboy-swagger/pull/8) ([cabol](https://github.com/cabol))
- Project and Swagger-UI setup. [\#7](https://github.com/inaka/cowboy-swagger/pull/7) ([cabol](https://github.com/cabol))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*