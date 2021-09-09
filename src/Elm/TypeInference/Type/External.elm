module Elm.TypeInference.Type.External exposing (mat4, texture, vec2, vec3, vec4)

import Elm.TypeInference.Type as Type exposing (Type)


vec2 : Type
vec2 =
    Type.external
        ( "Math", [ "Vector2" ] )
        "Vec2"


vec3 : Type
vec3 =
    Type.external
        ( "Math", [ "Vector3" ] )
        "Vec3"


vec4 : Type
vec4 =
    Type.external
        ( "Math", [ "Vector4" ] )
        "Vec4"


mat4 : Type
mat4 =
    Type.external
        ( "Math", [ "Matrix4" ] )
        "Mat4"


texture : Type
texture =
    Type.external
        ( "WebGL", [ "Texture" ] )
        "Texture"
