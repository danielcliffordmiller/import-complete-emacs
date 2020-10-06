CREATE TABLE IF NOT EXISTS projects (
       id INTEGER PRIMARY KEY,
       path TEXT UNIQUE ON CONFLICT IGNORE
);

CREATE TABLE IF NOT EXISTS packages (
       id INTEGER PRIMARY KEY,
       name TEXT UNIQUE ON CONFLICT IGNORE
);

CREATE TABLE IF NOT EXISTS classes (
       id INTEGER PRIMARY KEY,
       name TEXT UNIQUE ON CONFLICT IGNORE
);

CREATE TABLE IF NOT EXISTS project_package_class (
       id INTEGER PRIMARY KEY,
       project_id INTEGER,
       package_id INTEGER,
       class_id INTEGER
);

CREATE INDEX IF NOT EXISTS projects_idx ON projects ( path );
CREATE INDEX IF NOT EXISTS packages_idx ON packages ( name );
CREATE INDEX IF NOT EXISTS classes_idx ON classes ( name );

-- example on how to import new projects:
-- cat tags.json | jq -r 'to_entries | .[] | {class: .key, package: .value[]} | "INSERT INTO packages ( name ) VALUES ('\''"+.package+"'\'');\nINSERT INTO classes ( name ) VALUES ('\''"+.class+"'\'');\nINSERT INTO project_package_class (project_id,package_id,class_id) SELECT pr.id project_id, pa.id package_id, cl.id classes_id FROM projects pr JOIN packages pa JOIN classes cl WHERE pr.path = '\'~/projects/jvm-sql\'' AND pa.name = '\''"+.package+"'\'' AND cl.name = '\''"+.class+"'\'';"'
