module Elm.TypeInference.Type.External exposing (mat4, texture, vec2, vec3, vec4)

import Elm.TypeInference.Type as Type exposing (MonoType)


vec2 : MonoType
vec2 =
    Type.external
        "elm-explorations/linear-algebra"
        ( "Math", [ "Vector2" ] )
        "Vec2"


vec3 : MonoType
vec3 =
    Type.external
        "elm-explorations/linear-algebra"
        ( "Math", [ "Vector3" ] )
        "Vec3"


vec4 : MonoType
vec4 =
    Type.external
        "elm-explorations/linear-algebra"
        ( "Math", [ "Vector4" ] )
        "Vec4"


mat4 : MonoType
mat4 =
    Type.external
        "elm-explorations/linear-algebra"
        ( "Math", [ "Matrix4" ] )
        "Mat4"


texture : MonoType
texture =
    Type.external
        "elm-explorations/webgl"
        ( "WebGL", [ "Texture" ] )
        "Texture"
