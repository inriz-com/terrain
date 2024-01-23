/*
	WebGL2 3D Terrain Demo
	Copyright (C) 2024  Inriz.com
	
	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.
	
	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	
	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

const changelog = `\        
Inriz Webgl2 Terrain Demo - Changelog (v3.1)
==========================================================================================

Jan 10, 2022 - Added terrain multitexturing
             - Added normal map and specular highlights to water plane

Jan 10, 2022 - Added a flashlight at night

Jan 12, 2022 - Implemented a virtual fixed interval physics loop for consistency across devices
             - Modified water buoyancy
             - Modified movement controls for swimming

Jan 13, 2022 - Greatly improved performance 
             - Implemented water refraction and depth effects

Jan 14, 2022 - Implemented water reflection
             - Added Fresnel effect blending to water reflections and refractions

Feb 20, 2022 - Added normal maps and specular highlights
             - Changed textures

Feb 21, 2022 - Changed water fog curve to be equivelent when viewed from above or below the surface

Mar 27, 2022 - Changed terrain heightmap generation

Mar 29, 2022 - Added atmosphere and sun

Apr 1, 2022 - Added gamma correction

Apr 2, 2022 - Added terrain level of detail and chunks

Sep 27, 2022 - Added grass quads (no dir lighting/shadows yet)

Dec 30, 2023 - Added trees, rocks and foliage
             - Improved grass rendering  

`;


function begin(resolution, antialiasOn, renderDistanc) {

    const useFPBuffers = resolution.hdr;

    const RES_DIVIDER = 1;
    const WATER_RES_DIVIDER = 1;
    const DISP_SCALE = 1 / RES_DIVIDER;

    let ltx = 0;
    let lty = 0;

    const TERR_TRI_W = 400;
    const sttd = 1;

    let gameInput = true;
    let latestCommandEnterTime = 0;

    const mapSize = 5000;


    let map = new Float32Array(mapSize * mapSize);

    let lights = [{ pos: [1, 3000, 300], color: [1, 0.6, 0.3] }, { pos: [700, 800, 2000], color: [0, 1, 0] }, { pos: [700, 2000, 2000], color: [0, 0, 1] }];
    let ambientColor = [0.3, 0.3, 0.4];
    // ambientColor = [0.1, 0.1, 0.1];
    const DAY_SKY_COLOR = [0.5, 0.58, 0.95];
    const FLASHLIGHT_ON_COLOR = [0.8 * 9, 0.8 * 9, 0.7 * 9];

    let zenithColor = DAY_SKY_COLOR;
    let horizonColor = DAY_SKY_COLOR;
    // skyColor = [0.1, 0.1, 0.1];
    let skyDirColor = [0.8, 0.8, 0.9];
    // skyDirColor = [0.05, 0.05, 0.05];

    let sunDir = [0, 0, 0];

    let flashlightColor = FLASHLIGHT_ON_COLOR;

    let lightColor = [];
    let lightPos = [];

    let lightMoveSelection = 1;

    let keyDown = {};

    let playerX = 734349;//185640; // render space coord
    let playerY = 10721;
    let playerZ = 1255133;//163880;

    let pwx = 0; // 1 pw = 1600 render space coords (chunk x)
    let pwy = 0;
    let pwz = 0; // (chunk y)

    let playerXvel = 0;
    let playerYvel = 0;
    let playerZvel = 0;

    let jumpcount = 99999;

    let lastCCX = 0;
    let lastCCZ = 0;

    let underwater = false;


    let FP_CHUNK_SIZE = 3200000;

    let planePos = { x: 1455 + 10 * FP_CHUNK_SIZE, y: 1980 + 2 * FP_CHUNK_SIZE, z: 713 + 14 * FP_CHUNK_SIZE };
    let planeVel = { x: 0, y: 0, z: 0 };
    let planeRot = { x: 3.14, y: 0, z: 0 };
    let planeRotVel = { x: 0, y: 0, z: 0 };

    let playerHBV = 0;

    let playerAH = degToRad(131);
    let playerAV = degToRad(-5);
    let playerAR = 0;

    let headHeight = 300;

    let sensitivity = 7;


    let leftMouseDown = false;
    let rightMouseDown = false;


    let fogDivisor = 199000;
    let fogColor = DAY_SKY_COLOR;

    let tstval = 0;


    let pFPS = 0;
    let pFPSAv = 0;
    let pFPSCount = 0;

    let ljumpTime = 0;

    let frameTime = 0;
    let deltaTime = 0;
    let fpsAvStartTime = 0;
    let fpsAverage = 0;
    let framesDrawn = -1;

    let tod = 0.35; // time
    let fcount = 0;

    let stm = 1.0;

    let loadedChunks = [];
    let chunkRenderRadius = 12;
    let chunkRenderDiameter = chunkRenderRadius * 2;
    let chunkSize = 64; // 64 by 64 triangle pairs

    const WATER_CLIP_LEVEL = 0;
    const GRASS_ENABLED = true;
    const ATMSIZE = 600000;

    let flyMode = false;

    const NUM_TREE_SPECIES = 1;
    const NUM_FOLIAGE_SPECIES = 2;
    const NUM_UNDERWATER_SPECIES = 1;

    let models = [

        {
            src: "models/tree2/tree2.sbmf",
            textureSrc: "models/tree2/color.png",
            textureNormalSrc: "models/tree2/normal.png",
            scale: 60,
            offsetY: -300,
            scaleMin: 0.6,
            scaleMax: 1.5,
            collisionResolution: 200,
            backfaceCulling: false,
        },
        {
            src: "models/rocks/rock1/rock1.sbmf",
            textureSrc: "models/rocks/rock1/color.png",
            textureNormalSrc: "models/rocks/rock1/normal.png",
            scale: 200,
            offsetY: -200,
            scaleMin: 0.2,
            scaleMax: 1.5,
            collisionResolution: 200,
            backfaceCulling: true,
        },
        {
            src: "models/bush/bush.sbmf",
            textureSrc: "models/bush/fir.png",
            textureNormalSrc: "models/bush/firnormal.png",
            scale: 800,
            offsetY: -200,
            scaleMin: 0.6,
            scaleMax: 1.2,
            collisionResolution: null, // no collision
            backfaceCulling: false,
        },
        {
            src: "models/seaweed/seaweed.sbmf",
            textureSrc: "models/seaweed/color.png",
            textureNormalSrc: "models/seaweed/normal.png",
            scale: 14,
            offsetY: -50,
            scaleMin: 0.6,
            scaleMax: 1.0,
            collisionResolution: null, // no collision
            backfaceCulling: true,
        },


        // {
        //     src: "models/armchair/armchair.sbmf",
        //     textureSrc: "models/armchair/yellow.jpg",
        //     textureNormalSrc: "models/armchair/normal.jpg",
        //     scale: 1000,

        // },
    ];

    let materialLibs = {};

    let checkpointsReached;
    let movementRecord;
    let counterStarted;
    let counterStart;
    let lastABSMovementRecord;

    function resetAttempt() {
        checkpointsReached = 0;
        playerXvel = 0;
        playerYvel = 0;
        playerZvel = 0;
        playerX = 734349;//185640; // render space coord
        playerY = 10721;
        playerZ = 1255133;//163880;
        counterStarted = false;
        movementRecord = [];
        counterStart = 0;
        lastABSMovementRecord = 0;
    }

    resetAttempt();

    let checkpoints = [
        {
            x: 398763,
            y: 4903,
            z: 1397500,
            newTOD: null,
        },
        {
            x: 305936,
            y: 24903,
            z: 1243361,
            newTOD: 0.9,
        },
        {
            x: 138762,
            y: 24485,
            z: 1226848,
            newTOD: 0.68,
        },
        {
            x: 217074,
            y: -26300,
            z: 805121,
            newTOD: 0.5,
        },
    ];

    let vertexShaderSource = `#version 300 es
in vec4 a_position;
in vec3 a_brightness;
in vec3 a_normal;
in vec2 a_texcoord; //n

uniform mat4 u_view;
uniform mat4 u_projection;
uniform vec3 u_viewWorldPosition; // pos of camera
uniform mat4 u_world;
uniform float u_fcount; 

out vec4 v_light; 

out vec3 v_surfaceToView;

out vec3 v_normal;
out vec3 v_surfaceToLight;
out vec2 v_texcoord; //n
out vec3 v_swp;
out vec3 v_brightness;

// temp?
out vec3 v_viewDir;

out float v_fogDepth;

void main() {
  v_brightness = a_brightness;
  vec4 worldPosition = u_world * a_position;
  gl_Position = u_projection * u_view * worldPosition;

  v_texcoord = a_texcoord;

  v_normal = a_normal;

  vec3 surfaceWorldPosition = (worldPosition).xyz;

  v_swp = surfaceWorldPosition;
  v_surfaceToView = u_viewWorldPosition - surfaceWorldPosition;
  v_fogDepth = -(u_view*worldPosition).z;
  v_viewDir = vec3(u_view[0][2], u_view[1][2], u_view[2][2]);
}
`;

    let fragmentShaderSource = `#version 300 es
precision highp float;

// point lights
uniform vec3 u_pointLightPos[10];
uniform vec3 u_pointLightColor[10];
uniform uint u_numPointLights;

// dirLight

uniform vec3 u_dirLightDir;
uniform vec3 u_dirLightColor;



// Passed in from the vertex shader.
in vec2 v_texcoord;
in vec3 v_normal;
in vec3 v_surfaceToView;
in vec3 v_brightness;
in vec3 v_swp;
in float v_fogDepth;

in vec3 v_viewDir;

uniform uint u_texID;
uniform float u_clipHeight;
uniform bool u_clipAbove;
uniform bool u_clipEnabled;


uniform sampler2D u_tex0;
uniform sampler2D u_tex1;
uniform sampler2D u_tex2;
uniform sampler2D u_tex0normal;
uniform sampler2D u_tex1normal;
uniform sampler2D u_tex2normal;

uniform vec3 u_lightDirection;
uniform vec3 u_ambientLightColor;

out vec4 outColor;

void main() {

  float gamma = 2.2;

  if (u_clipEnabled) {
    if (u_clipAbove) {
        if (v_swp.y > u_clipHeight) {
            discard;
        }
    } else {
        if (v_swp.y < u_clipHeight) {
            discard;
        }
    }
  }

  
  
  vec4 tex0c = v_brightness.r < 0.95 ? texture(u_tex0, v_texcoord) : vec4(1);
  vec4 tex1c = v_brightness.r > 0.05 ? texture(u_tex1, v_texcoord) : vec4(1);
  vec4 tex2c = v_brightness.g > 0.05 ? texture(u_tex2, v_texcoord) : vec4(1);

  vec3 tex0cnormal = v_brightness.r < 0.95 ? texture(u_tex0normal, v_texcoord).rgb : vec3(0.0, 0.0, 1.0);
  vec3 tex1cnormal = v_brightness.r > 0.05 ? texture(u_tex1normal, v_texcoord).rgb : vec3(0.0, 0.0, 1.0);
  vec3 tex2cnormal = v_brightness.g > 0.05 ? texture(u_tex2normal, v_texcoord).rgb : vec3(0.0, 0.0, 1.0);


  vec3 normal = normalize(v_normal);

  vec3 normalMix = mix(mix(tex0cnormal, tex1cnormal, v_brightness.r), tex2cnormal, v_brightness.g);

  vec3 nmOff = (normalMix - 0.5) * 1.2;

  normal.z += nmOff.r; // inaccurate but ok ig
  normal.x += nmOff.g;
  normal = normalize(normal);

  vec3 surfaceToDirLightDirection = normalize(u_dirLightDir);
  vec3 surfaceToViewDirection = normalize(v_surfaceToView);

  float dirDiffuse = clamp(dot(normal, surfaceToDirLightDirection), 0., 1.);
  vec3 surfaceToPntLightDirection = u_pointLightPos[0] - v_swp; // dir to light
  float pointDiffuse = dot(normal, normalize(surfaceToPntLightDirection)) * 40000000. / pow(length(surfaceToPntLightDirection), 2.);

  pointDiffuse *= clamp((pow(dot(surfaceToViewDirection, normalize(v_viewDir)), 2.0) - 0.78) / 4., 0., 5.);
//   pointDiffuse = clamp(pointDiffuse, 0., 0.8);

  vec3 halfVector = normalize(surfaceToDirLightDirection + surfaceToViewDirection);
  float specular = pow(abs(clamp(dot(normal, halfVector), 0.0, 1.0)), 220.) * (v_brightness.g * 0.4 + (v_brightness.r) * 0.5) * 0.3;


  vec3 texColor = pow(mix(mix(tex0c, tex1c, v_brightness.r), tex2c, v_brightness.g).rgb, vec3(gamma));

  outColor = vec4(texColor * (dirDiffuse* u_dirLightColor + max(pointDiffuse*u_pointLightColor[0], 0.) + u_ambientLightColor), 1.0);
  outColor.rgb = outColor.rgb + clamp(specular * u_dirLightColor, 0.0, 0.6);
  
}
`;

    let modelVertexShaderSource = `#version 300 es
in vec4 a_position;
in vec3 a_brightness;
in vec3 a_normal;
in vec2 a_texcoord; //n

uniform mat4 u_view;
uniform mat4 u_projection;
uniform vec3 u_viewWorldPosition; // pos of camera
uniform mat4 u_world;
uniform float u_fcount; 

out vec4 v_light; 

out vec3 v_surfaceToView;

out vec3 v_normal;
out vec3 v_surfaceToLight;
out vec2 v_texcoord; //n
out vec3 v_swp;
out vec3 v_brightness;

// temp?
out vec3 v_viewDir;

out float v_fogDepth;

void main() {
    v_brightness = a_brightness;
    vec4 worldPosition = u_world * a_position;
    gl_Position = u_projection * u_view * worldPosition;

    v_texcoord = a_texcoord;

    v_normal = a_normal;

    // we need to transform the normal's orientation to world space with u_world
    // because it's currently in model space
    v_normal = mat3(u_world) * v_normal;

    vec3 surfaceWorldPosition = (worldPosition).xyz;

    v_swp = surfaceWorldPosition;
    v_surfaceToView = u_viewWorldPosition - surfaceWorldPosition;
    v_fogDepth = -(u_view*worldPosition).z;
    v_viewDir = vec3(u_view[0][2], u_view[1][2], u_view[2][2]);
}
`;

    let modelFragmentShaderSource = `#version 300 es
    precision highp float;
    
    // point lights
    uniform vec3 u_pointLightPos[10];
    uniform vec3 u_pointLightColor[10];
    uniform uint u_numPointLights;
    
    // dirLight
    
    uniform vec3 u_dirLightDir;
    uniform vec3 u_dirLightColor;
    
    // Passed in from the vertex shader.
    in vec2 v_texcoord;
    in vec3 v_normal;
    in vec3 v_surfaceToView;
    in vec3 v_brightness;
    in vec3 v_swp;
    in float v_fogDepth;
    
    in vec3 v_viewDir;
    
    uniform sampler2D u_tex;
    uniform sampler2D u_tex_normal;
    
    uniform vec3 u_lightDirection;
    uniform vec3 u_ambientLightColor;
    
    out vec4 outColor;
    
    void main() {
    
        float gamma = 2.2;
    
        vec4 tex = texture(u_tex, v_texcoord);

        if (tex.a < 0.3) {
            discard;
        }
            
        vec3 tex_normal = texture(u_tex_normal, v_texcoord).rgb * 2.0 - 1.0; // convert from [0,1] to [-1,1]
        tex_normal *= 0.99; // scale normal strength

        vec3 normal = gl_FrontFacing ? v_normal : -v_normal;
    
        // compute the tangent basis matrix
        vec3 dp1 = dFdx(v_swp);
        vec3 dp2 = dFdy(v_swp);
        vec2 duv1 = dFdx(v_texcoord);
        vec2 duv2 = dFdy(v_texcoord);
    
        float invdet = 1.0 / (duv1.x * duv2.y - duv1.y * duv2.x);
    
        vec3 tangent = (dp1 * duv2.y - dp2 * duv1.y) * invdet;
        vec3 bitangent = (dp2 * duv1.x - dp1 * duv2.x) * invdet;
    
        mat3 TBN = mat3(normalize(tangent), normalize(bitangent), normalize(normal));
    
        // apply text_normal normal map to the vertex normal properly so that it works in all directions
        vec3 world_normal = normalize(TBN * tex_normal); // transform to world space
    
    
        vec3 surfaceToDirLightDirection = normalize(u_dirLightDir);
        vec3 surfaceToViewDirection = normalize(v_surfaceToView);
    
        float dirDiffuse = clamp(dot(world_normal, surfaceToDirLightDirection), 0., 1.);
        vec3 surfaceToPntLightDirection = u_pointLightPos[0] - v_swp; // dir to light
        float pointDiffuse = dot(world_normal, normalize(surfaceToPntLightDirection)) * 40000000. / pow(length(surfaceToPntLightDirection), 2.);
    
        pointDiffuse *= clamp((pow(dot(surfaceToViewDirection, normalize(v_viewDir)), 2.0) - 0.78) / 4., 0., 5.);
        //   pointDiffuse = clamp(pointDiffuse, 0., 0.8);
    
        vec3 halfVector = normalize(surfaceToDirLightDirection + surfaceToViewDirection);
        float specular = pow(abs(clamp(dot(world_normal, halfVector), 0.0, 1.0)), 220.) * (v_brightness.g * 0.4 + (v_brightness.r) * 0.5) * 0.3;
    
        outColor = vec4(tex.rgb * (dirDiffuse* u_dirLightColor + max(pointDiffuse*u_pointLightColor[0], 0.) + u_ambientLightColor), 1.0);
        outColor.rgb = outColor.rgb + clamp(specular * u_dirLightColor, 0.0, 0.6);
      
    }    
`;


    let waterVertexShaderSource = `#version 300 es
in vec4 a_position;

uniform mat4 u_view;
uniform mat4 u_projection;
uniform vec3 u_viewWorldPosition; // pos of camera
uniform mat4 u_world;
uniform mat4 u_textureMatrix;


out float v_fogDepth;
out vec3 v_surfaceToView;
out vec3 v_wp;
out vec4 v_clipSpace;

void main() {
  vec4 worldPosition = u_world * a_position;
  v_clipSpace = u_projection * u_view * worldPosition;
  gl_Position = v_clipSpace;
  v_fogDepth = -(u_view*worldPosition).z;
  vec3 surfaceWorldPosition = worldPosition.xyz;
  v_wp = surfaceWorldPosition;
  v_surfaceToView = u_viewWorldPosition - surfaceWorldPosition;
}
`;

    let waterFragmentShaderSource = `#version 300 es
precision highp float;

in float v_fogDepth;
in vec3 v_surfaceToView;
in vec3 v_wp;

uniform sampler2D u_texRipple;
uniform sampler2D u_refraction;
uniform sampler2D u_refractionDepth;
uniform sampler2D u_reflection;

uniform float u_fogDivisor; // 35000
uniform vec3 u_fogColor; //0.8, 0.8, 0.95

uniform vec3 u_dirLightDir;
uniform float u_fcount; 
uniform vec3 u_ambientLightColor;

in vec4 v_clipSpace;

out vec4 outColor;

float toDepth(float dbf) {
    float z_n = 2.0 * dbf - 1.0;
    return 2.0 * 100.0 * 650000.0 / (650000.0 + 100.0 - z_n * (650000.0 - 100.0));
}

void main() {
  float gamma = 2.2;

  float ripDiv = 12000.0;
  vec3 surfaceToViewDirection = normalize(v_surfaceToView);
  vec4 nb = texture(u_texRipple, vec2(v_wp.x / 9000.0 + mod(u_fcount / 700.0, 100.0), v_wp.z / ripDiv)) + texture(u_texRipple, vec2(v_wp.z / ripDiv + 100.0 + mod(u_fcount / 750.0, 100.0), v_wp.x / ripDiv + 120.0));
  float fresnel = abs(dot(vec3(0.0, 1.0, 0.0), surfaceToViewDirection));

  vec2 projectedTexcoord = v_clipSpace.xy / v_clipSpace.w / 2.0 + 0.5;

  vec2 puv = projectedTexcoord;
  vec2 puvDelta = (nb.xy - 1.0) / 48.0;
      
  float waterDepthActual = (toDepth(texture(u_refractionDepth, puv).r) - toDepth(gl_FragCoord.z));
  puvDelta *= clamp(waterDepthActual / 1200.0, 0.0, 1.0);
  vec3 normal = normalize(vec3((puvDelta.r) * 2.8, 1.0, (puvDelta.g)*2.8));

  vec4 refractionColor = texture(u_refraction, puv + puvDelta);

  float waterDepth = (toDepth(texture(u_refractionDepth, puv + puvDelta).r) - toDepth(gl_FragCoord.z));
  vec4 texColor = vec4(1.0, 0.0, 0.0, 0.2);

  vec4 reflectionColor = texture(u_reflection, vec2(puv.x, 1.0 - puv.y) - puvDelta);

  outColor = mix(reflectionColor, vec4(mix(refractionColor.rgb, vec3(0.3, 0.6, 0.9) * u_ambientLightColor * 2.1, (clamp((waterDepth) / u_fogDivisor, 0.0, 1.0)) - 0.01), 1.0), ((fresnel) * 0.9 + 0.1));
  float twdepth = 200.0 + (puvDelta.x * 20.0);
  

  outColor.a = clamp((waterDepthActual + (puvDelta.x * 20.0)) / twdepth, 0.0, 1.0);

}
`;


    let imageVertexShaderSource = `#version 300 es
in vec2 a_position;
in vec2 a_texcoord;

uniform vec2 u_imagePos;
uniform vec2 u_resolution;
uniform sampler2D u_image;

out vec2 v_texcoord;

void main() {
  float aspect = u_resolution.x / u_resolution.y;
  ivec2 texSize = textureSize(u_image, 0);
  vec2 vpos = a_position;
  vpos.x /= aspect;
  vpos.x *= float(texSize.x) / float(texSize.y);

  v_texcoord = a_position;
  v_texcoord.y = 1.0 - v_texcoord.y;

  gl_Position = vec4(u_imagePos + vpos * 0.10 - 0.05, 1.0, 1.0);
}
`;

    let imageFragmentShaderSource = `#version 300 es
precision highp float;

uniform sampler2D u_image;
in vec2 v_texcoord;

out vec4 outColor;

void main() {
  vec4 imageColor = texture(u_image, v_texcoord);
  outColor = imageColor;
}
`;

    let atmosphereVertexShaderSource = `#version 300 es
in vec4 a_position;

uniform mat4 u_view;
uniform mat4 u_projection;
uniform mat4 u_world;
uniform vec3 u_viewWorldPosition; // pos of camera
uniform vec3 u_dirLightDir;

out vec3 v_swp;

void main() {
  vec4 worldPosition = u_world * a_position;
  gl_Position = u_projection * u_view * worldPosition;


  vec3 surfaceWorldPosition = (worldPosition).xyz;
  v_swp = surfaceWorldPosition; //normalize(u_viewWorldPosition - surfaceWorldPosition);
}
`;

    let atmosphereFragmentShaderSource = `#version 300 es
precision highp float;

// Passed in from the vertex shader.
in vec3 v_swp;

uniform vec3 u_dirLightDir;
uniform vec3 u_viewWorldPosition; // pos of camera
uniform vec3 u_horizonColor;
uniform vec3 u_zenithColor;
uniform vec3 u_sunColor;
uniform bool u_renderSkyObjects;

out vec4 outColor;

void main() {
    float gamma = 2.2;

    vec3 surfaceToViewDirection = normalize(u_viewWorldPosition - v_swp);

    // calculate the elevation from the surfaceToView vector
    float elevation = (dot(vec3(0.0, 1.0, 0.0), surfaceToViewDirection));

    outColor = vec4(mix(u_zenithColor, u_horizonColor, clamp(elevation * 0.6 + 0.3, 0.0, 1.0)), 0.3);
    // outColor = vec4(mix(vec3(0.254, 0.474, 0.882), vec3(0.937, 0.933, 0.882), clamp(elevation / 1.0 + 0.3, 0.0, 1.0)), 0.0);

    if (u_renderSkyObjects) {
        // add the sun (the dir light in the sky)
        float brt = pow(clamp(dot(u_dirLightDir, -surfaceToViewDirection) * 1.001, 0.0, 100000.0), 7506.6) * 1.2;
        outColor.rgb += clamp(brt * normalize(u_sunColor) * 2.0, 0.0, 80.0);
        outColor.rgb += u_sunColor * clamp(pow(clamp(dot(u_dirLightDir, -surfaceToViewDirection) * 1.01, 0.0, 100000.0), 2.6), 0.0, 7.0) * 0.3;
        outColor.rgb += u_sunColor * clamp(pow(clamp(dot(u_dirLightDir, -surfaceToViewDirection) * 1.01, 0.0, 100000.0), 25.0), 0.0, 7.0) * 0.3;
    }

}
`;


    let grassVertexShaderSource = `#version 300 es
in vec4 a_position;
in vec2 a_texcoord;

uniform mat4 u_view;
uniform mat4 u_projection;
uniform mat4 u_world;
uniform float u_fcount; 

out vec2 v_texcoord;
out vec3 v_swp;
out float v_fogDepth;
out vec3 v_viewDir;

float hash(vec2 p) {
    p = 50.0*fract(p * 0.3183099 + vec2(0.71, 0.113));
    return -1.0 + 2.0 * fract(p.x * p.y * (p.x + p.y));
}

void main() {
  vec4 apos = a_position;

  float u_fcount_d = u_fcount;
  
  vec4 worldPosition = u_world * (apos);
  float v = hash(vec2(worldPosition.x, worldPosition.z));
  worldPosition.xyz += (1.0 - a_texcoord.y) * vec3(sin(u_fcount_d / (60.0 + v * 10.0)) * 100.0, (sin(u_fcount_d /(70.0 + v * 10.0)) + 1.0 + (v * 2.0)) * 50.0, sin(u_fcount_d / (55.0 + v * 10.0)) * 100.0);
  gl_Position = u_projection * u_view * worldPosition;

  v_texcoord = a_texcoord;
  vec3 surfaceWorldPosition = (worldPosition).xyz;
  v_swp = surfaceWorldPosition;
  v_fogDepth = -(u_view*worldPosition).z;
  v_viewDir = vec3(u_view[0][2], u_view[1][2], u_view[2][2]);
}`;

    let grassFragmentShaderSource = `#version 300 es
precision highp float;

uniform vec3 u_dirLightDir;
uniform vec3 u_dirLightColor;
uniform vec3 u_viewWorldPosition; // pos of camera

// point lights
uniform vec3 u_pointLightPos[10];
uniform vec3 u_pointLightColor[10];

in vec2 v_texcoord;
in float v_fogDepth;
in vec3 v_swp;
in vec3 v_viewDir;

uniform float u_clipHeight;
uniform bool u_clipAbove;
uniform bool u_clipEnabled;

uniform sampler2D u_tex0;

uniform vec3 u_ambientLightColor;

out vec4 outColor;

void main() {

  float gamma = 2.2;

  if (u_clipEnabled) {
    if (u_clipAbove) {
        if (v_swp.y > u_clipHeight) {
            discard;
        }
    } else {
        if (v_swp.y < u_clipHeight) {
            discard;
        }
    }
  }

  vec4 tex = texture(u_tex0, v_texcoord);
  tex.rgb = pow(tex.rgb, vec3(gamma));
  if (tex.a < 0.8) { // 0.1 if using alpha blending?
    discard;
  }

  vec3 surfaceToViewDirection = normalize(u_viewWorldPosition - v_swp);

  vec3 surfaceToPntLightDirection = u_pointLightPos[0] - v_swp; // dir to light
  float pointDiffuse = 1.0 * 40000000.0 / pow(length(surfaceToPntLightDirection), 2.);
  pointDiffuse *= clamp((pow(dot(surfaceToViewDirection, normalize(v_viewDir)), 2.0) - 0.78) / 4., 0., 5.);

//   vec4 texColor = mix(vec3(0.14, 0.2, 0.1), vec3(0.07, 0.2, 0.02), v_texcoord.y);
  outColor = tex * vec4((u_dirLightColor + u_ambientLightColor + u_pointLightColor[0] * pointDiffuse), 1.0);
}`;




    let postProcFSQVertexShaderSource = `#version 300 es
in vec2 a_position;

out vec2 v_texcoord;

void main() {
  gl_Position = vec4(a_position.x, a_position.y, 0.0, 1.0);
  v_texcoord = (a_position + 1.0) / 2.0;
}
`;

    let postProcFinalFragmentShaderSource = `#version 300 es
precision highp float;

in vec2 v_texcoord;

uniform sampler2D u_imgIn;
uniform sampler2D u_imgDepthIn;

uniform int u_toneMappingSelection;

out vec4 outColor;

const float zNear = 100.0;
const float zFar = 650000.0;

float toDepth(float dbf) {
    float z_n = 2.0 * dbf - 1.0;
    return 2.0 * 100.0 * 650000.0 / (650000.0 + 100.0 - z_n * (650000.0 - 100.0));
}

float readDepth(vec2 coord) {
  if (v_texcoord.x < 0.0 || v_texcoord.y < 0.0)
    return 1.0;
  else {
    float z_b = texture(u_imgDepthIn, coord).x;
    return toDepth(z_b);
  }
}

vec3 simpleReinhardToneMapping(vec3 color) {
	float exposure = 1.5;
	color *= exposure / (1.0 + color / exposure);
	return color;
}

vec3 whitePreservingLumaBasedReinhardToneMapping(vec3 color) {
	float white = 2.0;
	float luma = dot(color, vec3(0.2126, 0.7152, 0.0722));
	float toneMappedLuma = luma * (1. + luma / (white*white)) / (1.0 + luma);
	color *= toneMappedLuma / luma;
	return color;
}

vec3 gUC2ToneMapping(vec3 color) {
	float A = 0.15;
	float B = 0.50;
	float C = 0.10;
	float D = 0.20;
	float E = 0.02;
	float F = 0.30;
	float W = 11.2;
	float exposure = 2.;
	color *= exposure;
	color = ((color * (A * color + C * B) + D * E) / (color * (A * color + B) + D * F)) - E / F;
	float white = ((W * (A * W + C * B) + D * E) / (W * (A * W + B) + D * F)) - E / F;
	color /= white;
	return color;
}

vec3 color_sat(vec3 color, float saturation) {
    vec3 orig = color;
    float luminance = color.r * 0.25 + color.g * 0.5 + color.b * 0.15;
    vec3 grayscale = vec3(luminance);
    return normalize(mix(grayscale, orig, saturation)) * luminance * 1.5;
}

vec3 color_contrast(vec3 color, float contrast) {
    return (color - 0.5) * (contrast) + 0.5;
}

float hash(vec2 p) {
    p = 50.0 * fract(p * 0.3183099 + vec2(0.71, 0.113));
    return fract(p.x * p.y * (p.x + p.y));
}

void main() {

  float gamma = 2.2;

  vec4 img = texture(u_imgIn, v_texcoord);

  outColor.rgb = img.rgb;
  outColor.a = 1.0;

  outColor.rgb = whitePreservingLumaBasedReinhardToneMapping(outColor.rgb);

  

  switch (u_toneMappingSelection) {
    case 1:
        outColor.rgb += hash(v_texcoord * 4000.0) / 255.0;
        break;
    case 2:
        vec3 orig = outColor.rgb;
        float luminance = outColor.r * 0.25 + outColor.g * 0.5 + outColor.b * 0.15;
        vec3 grayscale = vec3(luminance);
        float saturation = clamp(luminance * 140.0, 0.1, 1.0);
        outColor.rgb = normalize(mix(grayscale, orig, saturation)) * luminance * 1.5;

        break;
    case 3:
        // outColor.rgb = outColor.rgb * 2.0;
        outColor.rgb += hash(v_texcoord * 4000.0) / 150.0;
        vec3 origc = outColor.rgb;
        outColor.rgb = color_contrast(outColor.rgb, 0.95);
        outColor.rgb = floor(outColor.rgb * 20.0) / 20.0;
        outColor.rgb = mix(outColor.rgb, origc, 0.65);
        outColor.rgb = color_sat(outColor.rgb, 0.96);
        break;
  }

  

  outColor.rgb = pow(outColor.rgb, vec3(1.0 / gamma));
}

`;

    let postProcFogFragmentShaderSource = `#version 300 es
precision highp float;

in vec2 v_texcoord;

uniform sampler2D u_imgIn;
uniform sampler2D u_imgDepthIn;
uniform sampler2D u_imgAtmosphereIn;

uniform float u_fogDivisor;
uniform vec3 u_fogColor;
uniform bool u_useFogColor;

out vec4 outColor;

const float zNear = 100.0;
const float zFar = 650000.0;

float toDepth(float dbf) {
    float z_n = 2.0 * dbf - 1.0;
    return 2.0 * 100.0 * 650000.0 / (650000.0 + 100.0 - z_n * (650000.0 - 100.0));
}


void main() {
  vec4 img = texture(u_imgIn, v_texcoord);
  float imgDepth = texture(u_imgDepthIn, v_texcoord).x;

  vec4 atmosphereColor = u_useFogColor ? vec4(u_fogColor, 1.0) : texture(u_imgAtmosphereIn, v_texcoord);
  outColor.rgb = img.rgb;

  float ffactor = 1.0 - clamp(exp(-pow(toDepth(imgDepth) / u_fogDivisor, 2.0)), 0.0, 1.0);

  if (img.a > 0.5)
    outColor.rgb = mix(img.rgb, clamp(atmosphereColor.rgb, 0.0, 1.0), ffactor);

  outColor.a = 1.0;
//   gl_FragDepth = imgDepth;
}
`;

    // TODO: render refraction terrain sand texture without specular highlights

    const loadingSP = showScrollPanel("");
    loadingSP.innerText += "Generating terrain...\nNot loading? Make sure your browser supports WebGL2 and you have a relatively modern GPU.\n";
    setTimeout(genHeightMap, 1);

    const canvas = document.getElementById("canvas");
    const gl = window._gl = canvas.getContext("webgl2", {
        antialias: antialiasOn,
        desynchronized: true,
        alpha: false,
        preserveDrawingBuffer: true,
    });


    if (!gl)
        alert('Your browser/device does not support WebGL2 and therefore cannot run this software.');

    sizeCanvas();

    if (useFPBuffers && !gl.getExtension("EXT_color_buffer_half_float")) {
        alert("16-bit float color buffer not available. Please use a relatively modern GPU/device.");
        window.location.reload();
        return;
    }

    const aspect = gl.canvas.width / gl.canvas.height;
    const projectionMatrix = mat4.perspective(degToRad(80), aspect, 100, 650000);
    const cameraPosition = [0, 0, 0];
    const target = [0, 0, 0];
    const up = [0, 1, 0];
    let cameraMatrix = mat4.lookAt(cameraPosition, target, up);


    function adjustUICont() {
        const res = { w: gl.canvas.width, h: gl.canvas.height };
        const rresr = res.w / res.h;
        const wresr = window.innerWidth / window.innerHeight;

        if (wresr > rresr) {
            const c = '0px ' + ((window.innerWidth - (res.w * window.innerHeight / res.h)) / 2) + 'px';
            document.getElementById('uicont').style.margin = c;
            document.getElementById('uicont').style.width = (res.w * window.innerHeight / res.h) + 'px';
            document.getElementById('uicont').style.height = '100%';
        } else {
            const c = '' + ((window.innerHeight - (res.h * window.innerWidth / res.w)) / 2) + 'px 0px';
            document.getElementById('uicont').style.margin = c;
            document.getElementById('uicont').style.height = (res.h * window.innerWidth / res.w) + 'px';
            document.getElementById('uicont').style.width = '100%';
        }
    }

    adjustUICont();
    window.addEventListener('resize', adjustUICont);


    let terrainProg = new function ShaderProgram() {
        this.prog = createShaderProgram(vertexShaderSource, fragmentShaderSource).prog;
        this.positionAttributeLocation = gl.getAttribLocation(this.prog, "a_position");
        this.brightnessAttributeLocation = gl.getAttribLocation(this.prog, "a_brightness");
        this.normalLocation = gl.getAttribLocation(this.prog, "a_normal");
        this.texcoordAttributeLocation = gl.getAttribLocation(this.prog, "a_texcoord");

        this.ambientLightColorLocation = gl.getUniformLocation(this.prog, "u_ambientLightColor");
        this.pointLightPosLocation = gl.getUniformLocation(this.prog, "u_pointLightPos");
        this.pointLightColorLocation = gl.getUniformLocation(this.prog, "u_pointLightColor");
        this.numPointLightsLocation = gl.getUniformLocation(this.prog, "u_numPointLights");

        // dir light
        this.dirLightDirLocation = gl.getUniformLocation(this.prog, "u_dirLightDir");
        this.dirLightColorLocation = gl.getUniformLocation(this.prog, "u_dirLightColor");

        this.viewMatrixLocation = gl.getUniformLocation(this.prog, "u_view");
        this.projectionMatrixLocation = gl.getUniformLocation(this.prog, "u_projection");
        this.viewWorldPositionLocation = gl.getUniformLocation(this.prog, "u_viewWorldPosition");
        this.worldPositionLocation = gl.getUniformLocation(this.prog, "u_world");

        // this.fogDivisorLocation = gl.getUniformLocation(this.prog, "u_fogDivisor");
        // this.fogColorLocation = gl.getUniformLocation(this.prog, "u_fogColor");

        this.clipHeightLocation = gl.getUniformLocation(this.prog, "u_clipHeight");
        this.clipAboveLocation = gl.getUniformLocation(this.prog, "u_clipAbove");
        this.clipEnabledLocation = gl.getUniformLocation(this.prog, "u_clipEnabled");

        this.fcountLocation = gl.getUniformLocation(this.prog, "u_fcount");
    }

    let modelProg = new function ShaderProgram() {
        this.prog = createShaderProgram(modelVertexShaderSource, modelFragmentShaderSource).prog;
        this.positionAttributeLocation = gl.getAttribLocation(this.prog, "a_position");
        this.brightnessAttributeLocation = gl.getAttribLocation(this.prog, "a_brightness");
        this.normalLocation = gl.getAttribLocation(this.prog, "a_normal");
        this.texcoordAttributeLocation = gl.getAttribLocation(this.prog, "a_texcoord");

        this.ambientLightColorLocation = gl.getUniformLocation(this.prog, "u_ambientLightColor");
        this.pointLightPosLocation = gl.getUniformLocation(this.prog, "u_pointLightPos");
        this.pointLightColorLocation = gl.getUniformLocation(this.prog, "u_pointLightColor");
        this.numPointLightsLocation = gl.getUniformLocation(this.prog, "u_numPointLights");

        // dir light
        this.dirLightDirLocation = gl.getUniformLocation(this.prog, "u_dirLightDir");
        this.dirLightColorLocation = gl.getUniformLocation(this.prog, "u_dirLightColor");

        this.viewMatrixLocation = gl.getUniformLocation(this.prog, "u_view");
        this.projectionMatrixLocation = gl.getUniformLocation(this.prog, "u_projection");
        this.viewWorldPositionLocation = gl.getUniformLocation(this.prog, "u_viewWorldPosition");
        this.worldPositionLocation = gl.getUniformLocation(this.prog, "u_world");

        // this.fogDivisorLocation = gl.getUniformLocation(this.prog, "u_fogDivisor");
        // this.fogColorLocation = gl.getUniformLocation(this.prog, "u_fogColor");

        // this.clipHeightLocation = gl.getUniformLocation(this.prog, "u_clipHeight");
        // this.clipAboveLocation = gl.getUniformLocation(this.prog, "u_clipAbove");
        // this.clipEnabledLocation = gl.getUniformLocation(this.prog, "u_clipEnabled");

        this.fcountLocation = gl.getUniformLocation(this.prog, "u_fcount");
    }

    let waterProg = new function ShaderProgram() {
        this.prog = createShaderProgram(waterVertexShaderSource, waterFragmentShaderSource).prog;
        this.positionAttributeLocation = gl.getAttribLocation(this.prog, "a_position");

        this.ambientLightColorLocation = gl.getUniformLocation(this.prog, "u_ambientLightColor");
        this.pointLightPosLocation = gl.getUniformLocation(this.prog, "u_pointLightPos");
        this.pointLightColorLocation = gl.getUniformLocation(this.prog, "u_pointLightColor");
        this.numPointLightsLocation = gl.getUniformLocation(this.prog, "u_numPointLights");

        // dir light
        this.dirLightDirLocation = gl.getUniformLocation(this.prog, "u_dirLightDir");
        this.dirLightColorLocation = gl.getUniformLocation(this.prog, "u_dirLightColor");

        this.viewMatrixLocation = gl.getUniformLocation(this.prog, "u_view");
        this.projectionMatrixLocation = gl.getUniformLocation(this.prog, "u_projection");
        this.viewWorldPositionLocation = gl.getUniformLocation(this.prog, "u_viewWorldPosition");
        this.worldPositionLocation = gl.getUniformLocation(this.prog, "u_world");

        this.fogDivisorLocation = gl.getUniformLocation(this.prog, "u_fogDivisor");
        this.fogColorLocation = gl.getUniformLocation(this.prog, "u_fogColor");

        this.dirLightDirLocation = gl.getUniformLocation(this.prog, "u_dirLightDir");
        this.fcountLocation = gl.getUniformLocation(this.prog, "u_fcount");
    }

    let atmosphereProg = new function ShaderProgram() {
        this.prog = createShaderProgram(atmosphereVertexShaderSource, atmosphereFragmentShaderSource).prog;
        this.positionAttributeLocation = gl.getAttribLocation(this.prog, "a_position");

        this.viewMatrixLocation = gl.getUniformLocation(this.prog, "u_view");
        this.projectionMatrixLocation = gl.getUniformLocation(this.prog, "u_projection");
        this.viewWorldPositionLocation = gl.getUniformLocation(this.prog, "u_viewWorldPosition");
        this.worldPositionLocation = gl.getUniformLocation(this.prog, "u_world");

        this.dirLightDirLocation = gl.getUniformLocation(this.prog, "u_dirLightDir");
        this.horizonColorLocation = gl.getUniformLocation(this.prog, "u_horizonColor");
        this.zenithColorLocation = gl.getUniformLocation(this.prog, "u_zenithColor");

        this.sunColorLocation = gl.getUniformLocation(this.prog, "u_sunColor");
        this.renderSkyObjectsLocation = gl.getUniformLocation(this.prog, "u_renderSkyObjects");
    }

    let imageProg = new function ShaderProgram() {
        this.prog = createShaderProgram(imageVertexShaderSource, imageFragmentShaderSource).prog;
        this.positionAttributeLocation = gl.getAttribLocation(this.prog, "a_position");
        this.imagePosLocation = gl.getUniformLocation(this.prog, "u_imagePos");
        this.imageTextureLocation = gl.getUniformLocation(this.prog, "u_image");
        this.imageViewportResLocation = gl.getUniformLocation(this.prog, "u_resolution");
    }

    let grassProg = new function ShaderProgram() {
        this.prog = createShaderProgram(grassVertexShaderSource, grassFragmentShaderSource).prog;
        this.positionAttributeLocation = gl.getAttribLocation(this.prog, "a_position");
        this.texcoordAttributeLocation = gl.getAttribLocation(this.prog, "a_texcoord");

        this.ambientLightColorLocation = gl.getUniformLocation(this.prog, "u_ambientLightColor");

        // dir light
        this.dirLightDirLocation = gl.getUniformLocation(this.prog, "u_dirLightDir");
        this.dirLightColorLocation = gl.getUniformLocation(this.prog, "u_dirLightColor");

        this.pointLightPosLocation = gl.getUniformLocation(this.prog, "u_pointLightPos");
        this.pointLightColorLocation = gl.getUniformLocation(this.prog, "u_pointLightColor");

        this.viewMatrixLocation = gl.getUniformLocation(this.prog, "u_view");
        this.projectionMatrixLocation = gl.getUniformLocation(this.prog, "u_projection");
        this.worldPositionLocation = gl.getUniformLocation(this.prog, "u_world");
        this.viewWorldPositionLocation = gl.getUniformLocation(this.prog, "u_viewWorldPosition");

        // this.fogDivisorLocation = gl.getUniformLocation(this.prog, "u_fogDivisor");
        // this.fogColorLocation = gl.getUniformLocation(this.prog, "u_fogColor");

        this.clipHeightLocation = gl.getUniformLocation(this.prog, "u_clipHeight");
        this.clipAboveLocation = gl.getUniformLocation(this.prog, "u_clipAbove");
        this.clipEnabledLocation = gl.getUniformLocation(this.prog, "u_clipEnabled");

        this.fcountLocation = gl.getUniformLocation(this.prog, "u_fcount");
    }

    let postProcFinalProg = new function ShaderProgram() {
        this.prog = createShaderProgram(postProcFSQVertexShaderSource, postProcFinalFragmentShaderSource).prog;
        this.positionAttributeLocation = gl.getAttribLocation(this.prog, "a_position");
        this.imgInLocation = gl.getUniformLocation(this.prog, "u_imgIn");
        this.imgDepthInLocation = gl.getUniformLocation(this.prog, "u_imgDepthIn");
        this.toneMappingSelectionLocation = gl.getUniformLocation(this.prog, "u_toneMappingSelection");
    }

    let postProcFogProg = new function ShaderProgram() {
        this.prog = createShaderProgram(postProcFSQVertexShaderSource, postProcFogFragmentShaderSource).prog;
        this.positionAttributeLocation = gl.getAttribLocation(this.prog, "a_position");
        this.imgInLocation = gl.getUniformLocation(this.prog, "u_imgIn");
        this.imgDepthInLocation = gl.getUniformLocation(this.prog, "u_imgDepthIn");
        this.imgAtmosphereInLocation = gl.getUniformLocation(this.prog, "u_imgAtmosphereIn");
        this.fogDivisorLocation = gl.getUniformLocation(this.prog, "u_fogDivisor");
        this.fogColorLocation = gl.getUniformLocation(this.prog, "u_fogColor");
        this.useFogColorLocation = gl.getUniformLocation(this.prog, "u_useFogColor");
    }


    const waterBuff = window._wbuff = {
        positionBuffer: gl.createBuffer(),
        brightnessBuffer: gl.createBuffer(),
        normalBuffer: gl.createBuffer(),
        texcoordBuffer: gl.createBuffer(),
    }

    const atmosphereBuff = {
        positionBuffer: gl.createBuffer(),
        numTriangles: 0,
    }

    const imageBuff = {
        positionBuffer: gl.createBuffer(),
        texcoordBuffer: gl.createBuffer(),
        numTriangles: 2,
    }

    const fullScreenQuadBuff = {
        positionBuffer: gl.createBuffer(),
    }


    let vao = gl.createVertexArray();
    gl.bindVertexArray(vao);

    let texLoaded = 0;
    let texWaitingToLoad = 0;

    function loadTexture(texture, path, anisotropicFilteringOn = false, mipmaps = true, linearFiltering = true) {
        texWaitingToLoad++;
        gl.bindTexture(gl.TEXTURE_2D, texture);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 1, 1, 0, gl.RGBA, gl.UNSIGNED_BYTE,
            new Uint8Array([255, 0, 127, 255]));

        let img = new Image();
        img.src = path;
        img.addEventListener('load', function () {
            texLoaded++;
            const filteringMode = mipmaps ? (linearFiltering ? gl.LINEAR_MIPMAP_NEAREST : gl.NEAREST_MIPMAP_NEAREST) : (linearFiltering ? gl.LINEAR : gl.NEAREST);
            gl.bindTexture(gl.TEXTURE_2D, texture);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, filteringMode);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, linearFiltering ? gl.LINEAR : gl.NEAREST);
            const ext = (
                gl.getExtension('EXT_texture_filter_anisotropic') ||
                gl.getExtension('MOZ_EXT_texture_filter_anisotropic') ||
                gl.getExtension('WEBKIT_EXT_texture_filter_anisotropic')
            );
            if (ext && anisotropicFilteringOn) {
                const max = gl.getParameter(ext.MAX_TEXTURE_MAX_ANISOTROPY_EXT);
                gl.texParameterf(gl.TEXTURE_2D, ext.TEXTURE_MAX_ANISOTROPY_EXT, max);
            }
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, img);
            gl.generateMipmap(gl.TEXTURE_2D);
            ldone();
            loadingSP.innerText += "Loaded " + texLoaded + " of " + texWaitingToLoad + " textures...\n";
        });

    }


    let textureGrass = gl.createTexture();
    loadTexture(textureGrass, "./pbrtextures/grassc/albedo.jpg", true);

    let textureGrassNormal = gl.createTexture();
    loadTexture(textureGrassNormal, "./pbrtextures/grassc/normal.jpg", false);

    let textureBeach = gl.createTexture();
    loadTexture(textureBeach, "./pbrtextures/sand/albedo.jpg", true);

    let textureBeachNormal = gl.createTexture();
    loadTexture(textureBeachNormal, "./pbrtextures/sand/normal.jpg", false);

    let textureRock = gl.createTexture();
    loadTexture(textureRock, "./pbrtextures/rock/albedo.jpg", true);

    let textureRockNormal = gl.createTexture();
    loadTexture(textureRockNormal, "./pbrtextures/rock/normal.jpg", false);

    let textureWaterDM = gl.createTexture();
    loadTexture(textureWaterDM, "./water-dmap.png", false);

    let textureGrassBB = gl.createTexture();
    loadTexture(textureGrassBB, "./grassBB2.png", false, true, false);

    let textureUICheckpoint = gl.createTexture();
    loadTexture(textureUICheckpoint, "./checkpoint.png", false);

    const renderBuffer1 = createBuffer(gl.canvas.width, gl.canvas.height, false);
    const renderBuffer2 = createBuffer(gl.canvas.width, gl.canvas.height, false);
    const atmosphereFramebuffer = createBuffer(gl.canvas.width, gl.canvas.height, false);


    // send the water plane geometry to the GPU
    {
        let size = 490000;
        let w = {
            positions: [
                0, 0, 0,
                0, 0, size,
                size, 0, size,
                0, 0, 0,
                size, 0, size,
                size, 0, 0
            ],
            brightnesses: multValF32(6, [0.2, 0.2, 0.9], true),
            normals: multValF32(6, [0, 1, 0]),
            texcoords: [
                0.0, 0.0,
                1.0, 0.0,
                1.0, 1.0,
                0.0, 0.0,
                1.0, 1.0,
                0.0, 1.0,
            ]
        };


        // Send Vert Positions
        gl.bindBuffer(gl.ARRAY_BUFFER, waterBuff.positionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(w.positions), gl.STATIC_DRAW);

        // Send Vert Brightness Values
        gl.bindBuffer(gl.ARRAY_BUFFER, waterBuff.brightnessBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(w.brightnesses), gl.STATIC_DRAW);

        // Send Vert Normals
        gl.bindBuffer(gl.ARRAY_BUFFER, waterBuff.normalBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(w.normals), gl.STATIC_DRAW);

        // Send Texture Coord Normals
        gl.bindBuffer(gl.ARRAY_BUFFER, waterBuff.texcoordBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(w.texcoords), gl.STATIC_DRAW);
    }

    // send the atmosphere cube geometry to the GPU
    {
        // specify the vertices of a cube to surround the world and player
        let size = ATMSIZE;
        let a = {
            positions: cubeData(size).positions,
        }

        // Send Vert Positions
        gl.bindBuffer(gl.ARRAY_BUFFER, atmosphereBuff.positionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(a.positions), gl.STATIC_DRAW);
        atmosphereBuff.numTriangles = a.positions.length / 9;
    }

    // send the full screen quad geometry to the GPU
    {
        let w = {
            positions: [
                -1, -1,
                -1, 1,
                1, 1,
                -1, -1,
                1, 1,
                1, -1
            ],
        };

        // Send Vert Positions
        gl.bindBuffer(gl.ARRAY_BUFFER, fullScreenQuadBuff.positionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(w.positions), gl.STATIC_DRAW);
    }

    // send the image quad geometry to the GPU
    {
        let w = {
            positions: [
                0, 0,
                0, 1,
                1, 1,
                0, 0,
                1, 1,
                1, 0
            ],
        };

        // Send Vert Positions
        gl.bindBuffer(gl.ARRAY_BUFFER, imageBuff.positionBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(w.positions), gl.STATIC_DRAW);
        gl.bindBuffer(gl.ARRAY_BUFFER, imageBuff.texcoordBuffer);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(w.positions), gl.STATIC_DRAW);
    }


    gl.useProgram(terrainProg.prog);

    const u_image0Location = gl.getUniformLocation(terrainProg.prog, "u_tex0");
    const u_image1Location = gl.getUniformLocation(terrainProg.prog, "u_tex1");
    const u_image2Location = gl.getUniformLocation(terrainProg.prog, "u_tex2");
    const u_image0normalLocation = gl.getUniformLocation(terrainProg.prog, "u_tex0normal");
    const u_image1normalLocation = gl.getUniformLocation(terrainProg.prog, "u_tex1normal");
    const u_image2normalLocation = gl.getUniformLocation(terrainProg.prog, "u_tex2normal");

    gl.uniform1i(u_image0Location, 0);  // texture unit 0
    gl.uniform1i(u_image1Location, 1);  // texture unit 1
    gl.uniform1i(u_image2Location, 2);  // texture unit 1
    gl.uniform1i(u_image0normalLocation, 7);  // texture unit 7
    gl.uniform1i(u_image1normalLocation, 8);  // texture unit 8
    gl.uniform1i(u_image2normalLocation, 9);  // texture unit 9

    const refractionBuffer = createBuffer(gl.canvas.width / WATER_RES_DIVIDER, gl.canvas.height / WATER_RES_DIVIDER, false);
    const reflectionBuffer = createBuffer(gl.canvas.width / WATER_RES_DIVIDER, gl.canvas.height / WATER_RES_DIVIDER, false);
    const reflectionBuffer2 = createBuffer(gl.canvas.width / WATER_RES_DIVIDER, gl.canvas.height / WATER_RES_DIVIDER, false);

    gl.useProgram(grassProg.prog);
    const grass_u_image0Location = gl.getUniformLocation(grassProg.prog, "u_tex0");
    gl.uniform1i(grass_u_image0Location, 0);

    gl.useProgram(waterProg.prog);
    const u_imageRippleLocation = gl.getUniformLocation(waterProg.prog, "u_texRipple");
    const u_imageRefractionLocation = gl.getUniformLocation(waterProg.prog, "u_refraction");
    const u_imageRefractionDepthLocation = gl.getUniformLocation(waterProg.prog, "u_refractionDepth");
    const u_imageReflectionLocation = gl.getUniformLocation(waterProg.prog, "u_reflection");
    gl.uniform1i(u_imageRippleLocation, 3);
    gl.uniform1i(u_imageRefractionLocation, 4);
    gl.uniform1i(u_imageRefractionDepthLocation, 5);
    gl.uniform1i(u_imageReflectionLocation, 6);

    function ldone() {
        if (texLoaded !== texWaitingToLoad)
            return;

        loadingSP.close();
    }

    loadModels();


    /*
    
       <type = model>{
                src: "models/boat",
                parts: [ // parts of the model
                    {
                        name: 'unknown part',
                        mtllibName: '',
                        modelBuff: {
                            positionBuffer: null,
                            brightnessBuffer: null,
                            normalBuffer: null,
                            texcoordBuffer: null,
                        }
                    }
                ]
            }
    
    */

    function encodeSBMF(modelData) {
        // SBMF is a very basic 3d model format and only has one part and stores positions, texcoords and normals in separate Float32 arrays
        // it is a binary format
        // it is non indexed

        // header starts with 0 'S' 'B' 'M' 'F'
        // next 4 bytes are the number of vertices
        // next bytes are the positions as float 32s
        // next bytes are the texcoords as float 32s
        // next bytes are the normals as float 32s

        let content = new Uint8Array(4 + 4 + modelData.positions.length * 4 + modelData.texcoords.length * 4 + modelData.normals.length * 4);

        let writeIndex = 0;
        content[writeIndex++] = 0;
        content[writeIndex++] = 83;
        content[writeIndex++] = 66;
        content[writeIndex++] = 77;

        // use a little endian view
        let view = new DataView(content.buffer, 0);

        // write the number of vertices
        view.setUint32(writeIndex, modelData.positions.length / 3, true);
        writeIndex += 4;

        // write the positions
        for (let i = 0; i < modelData.positions.length; i++) {
            view.setFloat32(writeIndex, modelData.positions[i], true);
            writeIndex += 4;
        }

        // write the texcoords
        for (let i = 0; i < modelData.texcoords.length; i++) {
            view.setFloat32(writeIndex, modelData.texcoords[i], true);
            writeIndex += 4;
        }

        // write the normals
        for (let i = 0; i < modelData.normals.length; i++) {
            view.setFloat32(writeIndex, modelData.normals[i], true);
            writeIndex += 4;
        }

        return content;
    }

    function parseSBMF(content, scale = 1) {
        // SBMF is a very basic 3d model format and only has one part and stores positions, texcoords and normals in separate Float32 arrays
        // it is a binary format
        // it is non indexed

        // header starts with 0 'S' 'B' 'M' 'F'
        // next 4 bytes are the number of vertices
        // next bytes are the positions as float 32s (x, y, z)
        // next bytes are the texcoords as float 32s (u, v)
        // next bytes are the normals as float 32s (x, y, z)

        let readIndex = 0;
        let headerBytes = content.slice(readIndex, readIndex + 4);
        readIndex += 4;
        if (headerBytes[0] !== 0 || headerBytes[1] !== 83 || headerBytes[2] !== 66 || headerBytes[3] !== 77) {
            throw new Error('Invalid SBMF file.');
        }

        // use a little endian view
        let view = new DataView(content.buffer, 0);

        // get the number of vertices
        let numVertices = view.getUint32(readIndex, true);
        readIndex += 4;

        // get the positions
        let positions = [];
        for (let i = 0; i < numVertices * 3; i++) {
            positions.push(view.getFloat32(readIndex, true) * scale);
            readIndex += 4;
        }

        // get the texcoords
        let texcoords = [];
        for (let i = 0; i < numVertices * 2; i++) {
            texcoords.push(view.getFloat32(readIndex, true));
            readIndex += 4;
        }

        // get the normals
        let normals = [];
        for (let i = 0; i < numVertices * 3; i++) {
            normals.push(view.getFloat32(readIndex, true));
            readIndex += 4;
        }


        return {
            positions,
            texcoords,
            normals,
        };

    }

    // console.log('SBMF test');
    // const modelData = {
    //     positions: [
    //         0, 0, 0,
    //         0, 0, 1,
    //         1, 0, 1,
    //         0, 0, 0,
    //         1, 0, 1,
    //         1, 0, 0
    //     ],
    //     texcoords: [
    //         0, 0,
    //         0, 1,
    //         1, 1,
    //         0, 0,
    //         1, 1,
    //         1, 0
    //     ],
    //     normals: [
    //         0, 1, 0,
    //         0, 1, 0,
    //         0, 1, 0,
    //         0, 1, 0,
    //         0, 1, 0,
    //         0, 1, 0
    //     ]
    // };
    // console.log(parseSBMF(encodeSBMF(modelData), 1));




    // function parseMtllibFile(text) {
    //     let lines = text.split("\n");
    //     let mtllib = {
    //         materials: [],
    //     };

    //     function stripFilePathPrefix(path) {
    //         // split on / or \
    //         const pathParts = path.split(/[\/\\]/);
    //         return pathParts[pathParts.length - 1];
    //     }

    //     let materialIndex = -1;
    //     for (let i = 0; i < lines.length; i++) {
    //         let lineElem = lines[i].trim().split(" ");
    //         if (lineElem[0].substring(0, 1) == '#') {
    //             continue;
    //         }
    //         switch (lineElem[0]) {
    //             case 'newmtl':
    //                 materialIndex++;
    //                 mtllib.materials.push({
    //                     name: lineElem[1],
    //                 });
    //                 break;
    //             case 'map_Kd': // diffuse map
    //                 mtllib.materials[materialIndex].map_Kd = stripFilePathPrefix(lineElem[1]);
    //                 break;
    //             case 'map_Ks': // specular map
    //                 mtllib.materials[materialIndex].map_Ks = stripFilePathPrefix(lineElem[1]);
    //                 break;
    //             case 'map_Ns': // specular map
    //                 mtllib.materials[materialIndex].map_Ns = stripFilePathPrefix(lineElem[1]);
    //                 break;
    //             case 'map_Bump': // bump map (normal map)
    //                 mtllib.materials[materialIndex].map_Bump = stripFilePathPrefix(lineElem[1]);
    //                 break;
    //             case 'map_d':
    //                 mtllib.materials[materialIndex].map_d = lineElem[1];
    //                 break;
    //             case 'Ka':
    //                 mtllib.materials[materialIndex].Ka = [+(lineElem[1]), +(lineElem[2]), +(lineElem[3])];
    //                 break;
    //             case 'Kd':
    //                 mtllib.materials[materialIndex].Kd = [+(lineElem[1]), +(lineElem[2]), +(lineElem[3])];
    //                 break;
    //             case 'Ks':
    //                 mtllib.materials[materialIndex].Ks = [+(lineElem[1]), +(lineElem[2]), +(lineElem[3])];
    //                 break;
    //             case 'Ke':
    //                 mtllib.materials[materialIndex].Ke = [+(lineElem[1]), +(lineElem[2]), +(lineElem[3])];
    //                 break;
    //             case 'Ns':
    //                 mtllib.materials[materialIndex].Ns = +(lineElem[1]);
    //                 break;
    //             case 'Ni':
    //                 mtllib.materials[materialIndex].Ni = +(lineElem[1]);
    //                 break;
    //             case 'd':
    //                 mtllib.materials[materialIndex].d = +(lineElem[1]);
    //                 break;
    //             case 'illum':
    //                 mtllib.materials[materialIndex].illum = +(lineElem[1]);
    //                 break;

    //             default:
    //                 console.log('unknown mtllib line: ', lineElem);
    //         }
    //     }

    //     console.log('mtllib: ', mtllib);
    //     return mtllib;
    // }


    function generateVoxelCollider(model, resolution) {
        // determine bounding box (xMin, xMax, yMin, yMax, zMin, zMax)
        let xMin = Infinity;
        let xMax = -Infinity;
        let yMin = Infinity;
        let yMax = -Infinity;
        let zMin = Infinity;
        let zMax = -Infinity;

        for (let i = 0; i < model.positions.length; i += 3) {
            let x = model.positions[i + 0];
            let y = model.positions[i + 1];
            let z = model.positions[i + 2];

            if (x < xMin) xMin = x;
            if (x > xMax) xMax = x;
            if (y < yMin) yMin = y;
            if (y > yMax) yMax = y;
            if (z < zMin) zMin = z;
            if (z > zMax) zMax = z;
        }

        // add a little padding to the bounding box (1 resolution unit wide)
        xMin -= resolution;
        xMax += resolution;
        yMin -= resolution;
        yMax += resolution;
        zMin -= resolution;
        zMax += resolution;


        // determine the size of the bounding box
        let xSize = xMax - xMin;
        let ySize = yMax - yMin;
        let zSize = zMax - zMin;

        // determine the number of voxels in each dimension
        let xNum = Math.ceil(xSize / resolution);
        let yNum = Math.ceil(ySize / resolution);
        let zNum = Math.ceil(zSize / resolution);

        // create the voxel array
        let voxels = new Uint8Array(xNum * yNum * zNum);

        // fill the voxel array
        // this method does not account for large triangles that may span multiple voxels
        for (let i = 0; i < model.positions.length; i += 3) {
            let x = model.positions[i + 0];
            let y = model.positions[i + 1];
            let z = model.positions[i + 2];

            let xIndex = Math.floor((x - xMin) / resolution);
            let yIndex = Math.floor((y - yMin) / resolution);
            let zIndex = Math.floor((z - zMin) / resolution);

            voxels[xIndex + yIndex * xNum + zIndex * xNum * yNum] = 1;
        }

        function pointInTriangle(px, py, pz, x1, y1, z1, x2, y2, z2, x3, y3, z3) {
            // determine the vectors from the point to the vertices
            let v0 = [x3 - x1, y3 - y1, z3 - z1];
            let v1 = [x2 - x1, y2 - y1, z2 - z1];
            let v2 = [px - x1, py - y1, pz - z1];

            // compute dot products
            let dot00 = dot(v0, v0);
            let dot01 = dot(v0, v1);
            let dot02 = dot(v0, v2);
            let dot11 = dot(v1, v1);
            let dot12 = dot(v1, v2);

            // compute barycentric coordinates
            let invDenom = 1 / (dot00 * dot11 - dot01 * dot01);
            let u = (dot11 * dot02 - dot01 * dot12) * invDenom;
            let v = (dot00 * dot12 - dot01 * dot02) * invDenom;

            // check if point is in triangle
            return (u >= 0) && (v >= 0) && (u + v < 1);
        }

        // fill the voxel array using triangle bresehnam's line algorithm type
        // this method accounts for large triangles that may span multiple voxels
        // for (let i = 0; i < model.positions.length; i += 9) {
        //     let x1 = model.positions[i + 0];
        //     let y1 = model.positions[i + 1];
        //     let z1 = model.positions[i + 2];

        //     let x2 = model.positions[i + 3];
        //     let y2 = model.positions[i + 4];
        //     let z2 = model.positions[i + 5];

        //     let x3 = model.positions[i + 6];
        //     let y3 = model.positions[i + 7];
        //     let z3 = model.positions[i + 8];

        //     // determine the bounding box of the triangle
        //     let xMin = Math.min(x1, x2, x3);
        //     let xMax = Math.max(x1, x2, x3);
        //     let yMin = Math.min(y1, y2, y3);
        //     let yMax = Math.max(y1, y2, y3);
        //     let zMin = Math.min(z1, z2, z3);
        //     let zMax = Math.max(z1, z2, z3);

        //     // determine the voxel indices of the bounding box
        //     let xMinIndex = Math.floor((xMin - xMin) / resolution);
        //     let xMaxIndex = Math.floor((xMax - xMin) / resolution);
        //     let yMinIndex = Math.floor((yMin - yMin) / resolution);
        //     let yMaxIndex = Math.floor((yMax - yMin) / resolution);
        //     let zMinIndex = Math.floor((zMin - zMin) / resolution);
        //     let zMaxIndex = Math.floor((zMax - zMin) / resolution);

        //     // fill the voxel array
        //     for (let xIndex = xMinIndex; xIndex <= xMaxIndex; xIndex++) {
        //         for (let yIndex = yMinIndex; yIndex <= yMaxIndex; yIndex++) {
        //             for (let zIndex = zMinIndex; zIndex <= zMaxIndex; zIndex++) {
        //                 // determine if the voxel is inside the triangle
        //                 let x = xIndex * resolution + xMin;
        //                 let y = yIndex * resolution + yMin;
        //                 let z = zIndex * resolution + zMin;

        //                 // determine if the voxel is inside the triangle
        //                 let inside = pointInTriangle(x, y, z, x1, y1, z1, x2, y2, z2, x3, y3, z3);

        //                 if (inside) {
        //                     voxels[xIndex + yIndex * xNum + zIndex * xNum * yNum] = 1;
        //                 }
        //             }
        //         }
        //     }
        // }


        return {
            voxels,
            xMin,
            xMax,
            yMin,
            yMax,
            zMin,
            zMax,
            xNum,
            yNum,
            zNum,
            resolution,
        };

    }

    function multMat4Vec4(mat, vec) {
        let result = [];
        for (let i = 0; i < 4; i++) {
            result.push(0);
            for (let j = 0; j < 4; j++) {
                result[i] += mat[i + j * 4] * vec[j];
            }
        }
        return result;
    }



    async function loadModels() {
        for (let i = 0; i < models.length; i++) {
            let obj = await (await fetch('./' + models[i].src)).arrayBuffer();
            let modelData = parseSBMF(new Uint8Array(obj), models[i].scale);

            gl.useProgram(modelProg.prog);

            models[i].positionBuffer = gl.createBuffer();
            models[i].brightnessBuffer = gl.createBuffer();
            models[i].normalBuffer = gl.createBuffer();
            models[i].texcoordBuffer = gl.createBuffer();

            models[i].positions = modelData.positions;
            models[i].normals = modelData.normals;
            models[i].texcoords = modelData.texcoords;

            // give brightness values to all the vertices
            let brightnesses = multValF32(models[i].positions.length, 0.2, false);
            models[i].brightnesses = brightnesses;


            const u_texLocation = gl.getUniformLocation(modelProg.prog, "u_tex");
            const u_tex_normalLocation = gl.getUniformLocation(modelProg.prog, "u_tex_normal");
            gl.uniform1i(u_texLocation, 0); // set texture unit 0 for u_tex
            gl.uniform1i(u_tex_normalLocation, 7); // set texture unit 7 for u_tex_normal

            // Send Vert Positions
            gl.bindBuffer(gl.ARRAY_BUFFER, models[i].positionBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(models[i].positions), gl.STATIC_DRAW);

            // Send Vert Brightness Values
            gl.bindBuffer(gl.ARRAY_BUFFER, models[i].brightnessBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(models[i].brightnesses), gl.STATIC_DRAW);

            // Send Vert Normals
            gl.bindBuffer(gl.ARRAY_BUFFER, models[i].normalBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(models[i].normals), gl.STATIC_DRAW);

            // Send Texture Coord Normals
            gl.bindBuffer(gl.ARRAY_BUFFER, models[i].texcoordBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(models[i].texcoords), gl.STATIC_DRAW);

            models[i].texture = gl.createTexture();
            loadTexture(models[i].texture, './' + models[i].textureSrc, true);

            models[i].textureNormal = gl.createTexture();
            loadTexture(models[i].textureNormal, './' + models[i].textureNormalSrc, false);

            if (models[i].collisionResolution) {
                models[i].collider = generateVoxelCollider(models[i], models[i].collisionResolution);
                console.log('Model Collider RAM Usage: ', models[i].collider.voxels.length * 1 / 1000 / 1000, ' MB');
            }

            console.log('Loaded model: ', models[i]);

        }

        requestAnimationFrame(drawScene);
    }



    function radToDeg(r) {
        return r * 180 / Math.PI;
    }

    function degToRad(d) {
        return d * Math.PI / 180;
    }

    let init = false;
    let cube = cubeData(100);
    let meshBuilding = false;
    let cubeNum = 0;

    updateCC();

    function generateChunk(chunk, levelOfDetail) {
        let thisChunkSize = chunkSize >> (levelOfDetail - 1);
        let positions = new Float32Array(3 * 3 * 2 * thisChunkSize * thisChunkSize);
        let normals = new Float32Array(3 * 3 * 2 * thisChunkSize * thisChunkSize);
        let texcoords = new Float32Array(3 * 2 * 2 * thisChunkSize * thisChunkSize);
        let brightnesses = new Float32Array(3 * 3 * 2 * thisChunkSize * thisChunkSize);

        let grassPositions = new Float32Array(3 * 3 * 2 * thisChunkSize * thisChunkSize * 20);
        let grassTexcoords = new Float32Array(3 * 3 * 2 * thisChunkSize * thisChunkSize * 20);

        let numGrass = 0;

        let ppointer = 0;
        let npointer = 0;
        let tpointer = 0;
        let bpointer = 0;

        let tempCn = 0;

        let triSize = TERR_TRI_W << (levelOfDetail - 1);

        let tri = {
            a: [
                0, 0, 0,
                0, 0, triSize,
                triSize, 0, triSize,
            ],
            b: [
                0, 0, 0,
                triSize, 0, triSize,
                triSize, 0, 0,
            ]
        };

        function addGrass(cwx, cwz, x, y, z, r) {
            const dxd = playerX - (cwx);
            const dzd = playerZ - (cwz);
            const distanceToPlayer = Math.sqrt(dxd * dxd + dzd * dzd);

            // if (distanceToPlayer > 12000) {
            //         return;
            // } else if (distanceToPlayer > 80000) {
            //     if (numGrass % 4 != 0)
            //         return;
            // } else if (distanceToPlayer > 40000) {
            //     if (numGrass % 2 != 0)
            //         return;
            // }

            let size = 500;
            let sizey = 600;

            let dx = Math.sin(r) * size / 2;
            let dz = Math.cos(r) * size / 2;
            let dxb = -dx;
            let dzb = -dz;

            let w = {
                positions: [
                    dxb + x, y, z + dzb,
                    dx + x, y, z + dz,
                    dx + x, sizey + y, z + dz,
                    dxb + x, y, z + dzb,
                    dx + x, sizey + y, z + dz,
                    dxb + x, sizey + y, z + dzb,
                ],
                texcoords: [
                    0.0, 1.0,
                    1.0, 1.0,
                    1.0, 0.0,
                    0.0, 1.0,
                    1.0, 0.0,
                    0.0, 0.0,
                ]
            };

            for (let i = 0; i < w.positions.length; i++) {
                grassPositions[i + numGrass * w.positions.length] = w.positions[i];
            }

            for (let i = 0; i < w.texcoords.length; i++) {
                grassTexcoords[i + numGrass * w.texcoords.length] = w.texcoords[i];
            }

            chunk.grassBuffer.numTriangles += 2;

            numGrass++;
        }

        let t = 0;
        let g = 0;

        for (let qx = 0; qx < thisChunkSize; qx++) {
            for (let qz = 0; qz < thisChunkSize; qz++) {
                let type = bQuad(qx, qz, triSize);

                if (type.x < 0.3 && type.y < 0.1) {

                    const offsetX = Math.random() * (500 << (levelOfDetail - 1));
                    const offsetZ = Math.random() * (500 << (levelOfDetail - 1));

                    let xxwqabs = ((qx << (levelOfDetail - 1)) + chunk.chunkX * chunkSize) * TERR_TRI_W + offsetX;
                    let zzwqabs = ((qz << (levelOfDetail - 1)) + chunk.chunkZ * chunkSize) * TERR_TRI_W + offsetZ;

                    let xxabs = qx * triSize + offsetX;
                    let zzabs = qz * triSize + offsetZ;

                    if (levelOfDetail >= 4)
                        continue;

                    let a = 0;
                    t++;

                    if ((t & 1) == 0 && GRASS_ENABLED) {
                        for (let i = 0; i < 4; i++) {
                            a += (Math.random() * 0.4 + 0.6) * 6.284 / 3;
                            g++;

                            if (levelOfDetail == 2 && g % 3 != 0)
                                continue;

                            if (levelOfDetail == 3 && g % 6 != 0)
                                continue;

                            const gx = Math.random() * TERR_TRI_W * 2;
                            const gz = Math.random() * TERR_TRI_W * 2;

                            addGrass(xxwqabs, zzwqabs, gx + xxabs, getTerrainHeight(xxwqabs + gx, zzwqabs + gz) - 50, gz + zzabs, a);
                        }
                    }
                }
            }
        }

        {
            // Send Vert Positions
            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.buffer.positionBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, (positions), gl.STATIC_DRAW);

            // Send Vert Brightness Values
            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.buffer.brightnessBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, (brightnesses), gl.STATIC_DRAW);

            // Send Vert Normals
            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.buffer.normalBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, (normals), gl.STATIC_DRAW);

            // // Send Texture Coord
            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.buffer.texcoordBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, (texcoords), gl.STATIC_DRAW);
        }

        {
            // Send Vert Positions
            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.grassBuffer.positionBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, (grassPositions), gl.STATIC_DRAW);

            // // Send Texture Coord
            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.grassBuffer.texcoordBuffer);
            gl.bufferData(gl.ARRAY_BUFFER, (grassTexcoords), gl.STATIC_DRAW);
        }

        function bQuad(xx, zz, quadWidth) {
            let xxabs = (xx << (levelOfDetail - 1)) + chunk.chunkX * chunkSize;
            let zzabs = (zz << (levelOfDetail - 1)) + chunk.chunkZ * chunkSize;
            let dif = 1 << (levelOfDetail - 1);

            let y00 = terrainVertHeight(xxabs, zzabs);
            let y10 = terrainVertHeight(xxabs + dif, zzabs);
            let y01 = terrainVertHeight(xxabs, zzabs + dif);
            let y11 = terrainVertHeight(xxabs + dif, zzabs + dif);

            const dtws = 0.1 * dif;

            const tsvy = ((xx) % (1 / dtws)) * dtws;
            const tsvx = ((zz) % (1 / dtws)) * dtws;

            let pOff = [[0, 0], [0, 1], [1, 1], [0, 0], [1, 1], [1, 0]];

            let texcoordsMp = {
                a: [
                    0.0 + tsvx, 0.0 + tsvy,
                    dtws + tsvx, 0.0 + tsvy,
                    dtws + tsvx, dtws + tsvy,
                ],
                b: [
                    0.0 + tsvx, 0.0 + tsvy,
                    dtws + tsvx, dtws + tsvy,
                    0.0 + tsvx, dtws + tsvy,
                ]
            };


            let aH = [y00, y01, y11];
            let bH = [y00, y11, y10];

            tempCn++;

            let type = null;

            let pos = getLocalSpace(xx * quadWidth, 0, zz * quadWidth);

            for (let t = 0; t < 2; t++) {
                for (let i = 0; i < 3; i++) {

                    const curH = (t & 1) == 0 ? aH : bH;
                    const triSel = (t & 1) == 0 ? tri.a : tri.b;
                    const pOffInd = (t & 1) == 0 ? i : i + 3;

                    let y = curH[i];
                    let x = xxabs + pOff[pOffInd][0] * dif;
                    let z = zzabs + pOff[pOffInd][1] * dif;

                    if (x == 0) x = 1;
                    if (z == 0) z = 1;

                    let vn0 = getNormalFromMesh(x, z, true);
                    let vn1 = getNormalFromMesh(x, z, false);
                    let vn2 = getNormalFromMesh(x, z - dif, true);
                    let vn3 = getNormalFromMesh(x - dif, z - dif, true);
                    let vn4 = getNormalFromMesh(x - dif, z - dif, false);
                    let vn5 = getNormalFromMesh(x - dif, z, false);
                    let nx = (vn0.x + vn1.x + vn2.x + vn3.x + vn4.x + vn5.x) / 6;
                    let ny = (vn0.y + vn1.y + vn2.y + vn3.y + vn4.y + vn5.y) / 6;
                    let nz = (vn0.z + vn1.z + vn2.z + vn3.z + vn4.z + vn5.z) / 6;

                    positions[ppointer] = (triSel[i * 3] + pos.x);
                    ppointer++;
                    positions[ppointer] = (triSel[i * 3 + 1] + pos.y + y);
                    ppointer++;
                    positions[ppointer] = (triSel[i * 3 + 2] + pos.z);
                    ppointer++;

                    let r = 0;
                    let g = 0;
                    let b = 0;

                    let steepness = 0.015 * Math.pow((Math.abs(nx) + Math.abs(nz)) / ny * 1.5, 9);
                    if (steepness > 1) steepness = 1;

                    const heightGradF = Math.max(Math.min((800 - y) / 800, 1), 0);

                    brightnesses[bpointer] = steepness;
                    bpointer++;
                    brightnesses[bpointer] = heightGradF;
                    bpointer++;
                    brightnesses[bpointer] = 0;
                    bpointer++;

                    normals[npointer] = (nx);
                    npointer++;
                    normals[npointer] = (ny);
                    npointer++;
                    normals[npointer] = (nz);
                    npointer++;

                    type = { x: steepness, y: heightGradF };
                }
            }

            for (let i = 0; i < texcoordsMp.a.length; i++) {
                texcoords[tpointer] = (texcoordsMp.a[i]);
                tpointer++;
            }
            for (let i = 0; i < texcoordsMp.b.length; i++) {
                texcoords[tpointer] = (texcoordsMp.b[i]);
                tpointer++;
            }

            return type;
        }
    }

    function rebuildChunk(cx, cz) {
        for (let i = 0; i < loadedChunks.length; i++) {
            let chunk = loadedChunks[i];
            if (chunk.chunkX == cx && chunk.chunkZ == cz) {
                generateChunk(chunk, chunk.levelOfDetail, distanceToPlayer);
                break;
            }
        }
    }

    function generateChunkTrees(xx, zz) {

        const prng = new PRNG(3272347 + xx * 1000 + zz);

        const trees = []; // array of tree positions (x and z)

        const numTrees = Math.floor(prng.get() * 12) + 12;
        for (let i = 0; i < numTrees; i++) {
            const x = Math.floor(prng.get() * (chunkSize * TERR_TRI_W));
            const z = Math.floor(prng.get() * (chunkSize * TERR_TRI_W));
            const terrinf = getTerrainHeight(x + xx * chunkSize * TERR_TRI_W, z + zz * chunkSize * TERR_TRI_W, true);
            const y = terrinf.h;
            const type = Math.floor(prng.get() * NUM_TREE_SPECIES);
            const rotation = prng.get() * Math.PI * 2;
            const scale = models[type].scaleMin + prng.get() * (models[type].scaleMax - models[type].scaleMin);

            if (Math.abs(terrinf.n.x) < 0.3 && Math.abs(terrinf.n.z) < 0.3 && y > 0)
                trees.push({ x, y, z, type, rotation, scale });
        }

        const rocks = []; // array of rock positions (x and z)

        const numRocks = Math.floor(prng.get() * 12) + 12;
        for (let i = 0; i < numRocks; i++) {
            const x = Math.floor(prng.get() * (chunkSize * TERR_TRI_W));
            const z = Math.floor(prng.get() * (chunkSize * TERR_TRI_W));
            const terrinf = getTerrainHeight(x + xx * chunkSize * TERR_TRI_W, z + zz * chunkSize * TERR_TRI_W, true);
            const y = terrinf.h;
            const rotation = prng.get() * Math.PI * 2;
            const foliageType = Math.floor(prng.get() * NUM_FOLIAGE_SPECIES);
            const scale = models[NUM_TREE_SPECIES + foliageType].scaleMin + prng.get() * (models[NUM_TREE_SPECIES + foliageType].scaleMax - models[NUM_TREE_SPECIES + foliageType].scaleMin);

            if (Math.abs(terrinf.n.x) < 0.5 && Math.abs(terrinf.n.z) < 0.5 && y > 0)
                rocks.push({ x, y, z, type: NUM_TREE_SPECIES + foliageType, rotation, scale });
        }

        const underwaterFoliage = []; // array of rock positions (x and z)

        const numUnderwaterFoliage = Math.floor(prng.get() * 12) + 5;
        for (let i = 0; i < numUnderwaterFoliage; i++) {
            const x = Math.floor(prng.get() * (chunkSize * TERR_TRI_W));
            const z = Math.floor(prng.get() * (chunkSize * TERR_TRI_W));
            const terrinf = getTerrainHeight(x + xx * chunkSize * TERR_TRI_W, z + zz * chunkSize * TERR_TRI_W, true);
            const y = terrinf.h;
            const rotation = prng.get() * Math.PI * 2;

            const foliageType = Math.floor(prng.get() * NUM_UNDERWATER_SPECIES);
            const scale = models[NUM_TREE_SPECIES + NUM_FOLIAGE_SPECIES + foliageType].scaleMin + prng.get() * (models[NUM_TREE_SPECIES + NUM_FOLIAGE_SPECIES + foliageType].scaleMax - models[NUM_TREE_SPECIES + NUM_FOLIAGE_SPECIES + foliageType].scaleMin);


            if (Math.abs(terrinf.n.x) < 0.5 && Math.abs(terrinf.n.z) < 0.5 && y < -500 && y > -10000)
                underwaterFoliage.push({ x, y, z, type: NUM_TREE_SPECIES + NUM_FOLIAGE_SPECIES + foliageType, rotation, scale });
        }

        return { trees, rocks, underwaterFoliage };

    }

    function updateCC() {
        document.getElementById('status').innerText = 'Building terrain mesh...';

        let ppointer = 0;
        let bpointer = 0;
        let npointer = 0;
        let tpointer = 0;

        // concat data

        let pos = getPlayerPos();

        let xbMin = ~~(pos.x / TERR_TRI_W / chunkSize) - chunkRenderRadius;
        let xbMax = ~~(pos.x / TERR_TRI_W / chunkSize) + chunkRenderRadius;
        let zbMin = ~~(pos.z / TERR_TRI_W / chunkSize) - chunkRenderRadius;
        let zbMax = ~~(pos.z / TERR_TRI_W / chunkSize) + chunkRenderRadius;

        // if (xbMin < 0) xbMin = 0;
        // if (zbMin < 0) zbMin = 0;
        // if (xbMax > 1000) xbMax = 1000;
        // if (zbMax > 1000) zbMax = 1000;

        // delete all WebGL buffers for all chunks
        for (let i = 0; i < loadedChunks.length; i++) {
            let chunk = loadedChunks[i];

            if (chunk.chunkX < xbMin || chunk.chunkX > xbMax || chunk.chunkZ < zbMin || chunk.chunkZ > zbMax) {
                loadedChunks.splice(i, 1);
                i--;
                gl.deleteBuffer(chunk.buffer.positionBuffer);
                gl.deleteBuffer(chunk.buffer.normalBuffer);
                gl.deleteBuffer(chunk.buffer.texcoordBuffer);
                gl.deleteBuffer(chunk.buffer.brightnessBuffer);
            }
        }

        console.log('RL CHUnk: from ' + xbMin + ':' + zbMin + ' to ' + xbMax + ':' + zbMax);

        let xx = xbMin;
        let zz = zbMin;

        if (!meshBuilding) {
            meshBuilding = true;
            dq();
        }

        function dq() {
            if (xx >= xbMax) {
                zz++;
                xx = xbMin;
                if (zz >= zbMax) {
                    meshBuilding = false;
                    // cubeNum = tempCn;
                    // console.log('built ' + tempCn + ' quads');

                    if (playerX >= FP_CHUNK_SIZE) {
                        pwx++;
                        playerX -= FP_CHUNK_SIZE;
                    } else if (playerX < 0) {
                        if (pwx == 0) {
                            playerX = 0;
                            playerXvel = 0;
                        } else {
                            pwx--;
                            playerX += FP_CHUNK_SIZE;
                        }
                    }

                    if (playerZ >= FP_CHUNK_SIZE) {
                        pwz++;
                        playerZ -= FP_CHUNK_SIZE;
                    } else if (playerZ < 0) {
                        if (pwz == 0) {
                            playerZ = 0;
                            playerZvel = 0;
                        } else {
                            pwz--;
                            playerZ += FP_CHUNK_SIZE;
                        }
                    }

                    const n = [];
                    n.push(...loadedChunks);
                    console.log('generated: ', n);

                    // sort the loaded chunks based on their distance from the player
                    loadedChunks.sort((a, b) => {
                        const cScale = TERR_TRI_W * chunkSize;
                        const aChunkX = a.chunkX * cScale;
                        const aChunkZ = a.chunkZ * cScale;
                        const bChunkX = b.chunkX * cScale;
                        const bChunkZ = b.chunkZ * cScale;
                        const aChunkXDelta = aChunkX - playerX;
                        const aChunkZDelta = aChunkZ - playerZ;
                        const bChunkXDelta = bChunkX - playerX;
                        const bChunkZDelta = bChunkZ - playerZ;

                        const aDistSq = (aChunkXDelta * aChunkXDelta + aChunkZDelta * aChunkZDelta);
                        const bDistSq = (bChunkXDelta * bChunkXDelta + bChunkZDelta * bChunkZDelta);

                        return aDistSq - bDistSq;
                    });

                    console.log('sorted: ', loadedChunks);
                    document.getElementById('status').innerText = 'Running';
                    return;
                }

            }

            let levelOfDetail = 1;

            const chunkCenterWorldX = (xx + 0.5) * chunkSize * TERR_TRI_W;
            const chunkCenterWorldZ = (zz + 0.5) * chunkSize * TERR_TRI_W;
            const chunkCDX = (playerX - chunkCenterWorldX);
            const chunkCDZ = (playerZ - chunkCenterWorldZ);

            const distanceToPlayer = Math.sqrt(chunkCDX * chunkCDX + chunkCDZ * chunkCDZ);

            const l1 = 40000;
            const l2 = 60000;
            const l3 = 140000;
            const l4 = 280000;
            const l5 = 560000;

            if (distanceToPlayer > l5) {
                levelOfDetail = 6;
            } else if (distanceToPlayer > l4) {
                levelOfDetail = 5;
            } else if (distanceToPlayer > l3) {
                levelOfDetail = 4;
            } else if (distanceToPlayer > l2) {
                levelOfDetail = 3;
            } else if (distanceToPlayer > l1) {
                levelOfDetail = 2;
            }

            // levelOfDetail = 4;

            let foundExisting = false;
            for (let i = 0; i < loadedChunks.length; i++) {
                let chunk = loadedChunks[i];
                if (chunk.chunkX == xx && chunk.chunkZ == zz) {
                    if (chunk.levelOfDetail == levelOfDetail) {
                        foundExisting = true;
                        break;
                    } else {
                        loadedChunks.splice(i, 1);
                        gl.deleteBuffer(chunk.buffer.positionBuffer);
                        gl.deleteBuffer(chunk.buffer.normalBuffer);
                        gl.deleteBuffer(chunk.buffer.texcoordBuffer);
                        gl.deleteBuffer(chunk.buffer.brightnessBuffer);
                        break;
                    }

                }
            }

            if (!foundExisting) {
                const folliage = generateChunkTrees(xx, zz);
                let chunk = {
                    active: true,
                    chunkX: xx,
                    chunkZ: zz,
                    levelOfDetail: levelOfDetail,
                    trees: folliage.trees,
                    rocks: folliage.rocks,
                    underwaterFoliage: folliage.underwaterFoliage,
                    buffer: {
                        positionBuffer: gl.createBuffer(),
                        brightnessBuffer: gl.createBuffer(),
                        normalBuffer: gl.createBuffer(),
                        texcoordBuffer: gl.createBuffer()
                    },
                    grassBuffer: {
                        positionBuffer: gl.createBuffer(),
                        brightnessBuffer: gl.createBuffer(),
                        normalBuffer: gl.createBuffer(),
                        texcoordBuffer: gl.createBuffer(),
                        numTriangles: 0,
                    },
                };

                loadedChunks.push(chunk);
                generateChunk(chunk, chunk.levelOfDetail);
            }

            xx++;

            setTimeout(dq, 10);
        }
    }


    function terrainVertHeight(x, z) {
        return map[~~z * mapSize + ~~x] * 16000;
    }

    console.log(getTerrainHeight(5, 0));

    function getMeshCoord(x, z) {
        let gx = Math.floor(x / TERR_TRI_W); // get quad x
        let gz = Math.floor(z / TERR_TRI_W); // get quad z
        let s = x - gx * TERR_TRI_W < z - gz * TERR_TRI_W; // true if triangle with edge on mx = 0, mx = 1 ...; false if triangle edge on line gz = 0, gz = 1 ...
        return [gx, gz, s];
    }
    function getNormal(p0, p1, p2) {
        const u = { x: p1.x - p0.x, y: p1.y - p0.y, z: p1.z - p0.z };
        const v = { x: p2.x - p0.x, y: p2.y - p0.y, z: p2.z - p0.z };
        const normal = {
            x: u.y * v.z - u.z * v.y,
            y: u.z * v.x - u.x * v.z,
            z: u.x * v.y - u.y * v.x,
        }
        return normalizeV(normal);
    }


    function getTerrainHeight(x, z, gn = false) { // get the interpolated height for a coord on the terrain map
        function interpolateTriangle(
            p1 /*vec3*/,
            p2 /*vec3*/,
            p3 /*vec3*/,
            pos /*vec2*/
        ) {
            let det = (p2.z - p3.z) * (p1.x - p3.x) + (p3.x - p2.x) * (p1.z - p3.z);
            let l1 = ((p2.z - p3.z) * (pos.x - p3.x) + (p3.x - p2.x) * (pos.y - p3.z)) / det;
            let l2 = ((p3.z - p1.z) * (pos.x - p3.x) + (p1.x - p3.x) * (pos.y - p3.z)) / det;
            let l3 = 1 - l1 - l2;
            return l1 * p1.y + l2 * p2.y + l3 * p3.y;
        }

        let meshCoord = getMeshCoord(x, z);
        let mcx = meshCoord[0] * TERR_TRI_W;
        let mcz = meshCoord[1] * TERR_TRI_W;
        let mcs = meshCoord[2];
        let p1, p2, p3;
        if (mcs) { // z = 0 edged triangle
            p1 = { x: 0, y: terrainVertHeight(meshCoord[0], meshCoord[1]), z: 0 };
            p2 = { x: 0, y: terrainVertHeight(meshCoord[0], meshCoord[1] + 1), z: 0 + TERR_TRI_W };
            p3 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(meshCoord[0] + 1, meshCoord[1] + 1), z: 0 + TERR_TRI_W };
        } else {
            p1 = { x: 0, y: terrainVertHeight(meshCoord[0], meshCoord[1]), z: 0 };
            p2 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(meshCoord[0] + 1, meshCoord[1] + 1), z: 0 + TERR_TRI_W };
            p3 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(meshCoord[0] + 1, meshCoord[1]), z: 0 };
        }
        let pos = { x: x - mcx, y: z - mcz };
        if (gn) return { h: interpolateTriangle(p1, p2, p3, pos), n: getNormal(p1, p2, p3) };
        return interpolateTriangle(p1, p2, p3, pos);
    }

    function getNormalFromMesh(x, z, which) {
        let p1, p2, p3;
        if (which) { // z = 0 edged triangle |/
            p1 = { x: 0, y: terrainVertHeight(x, z), z: 0 };
            p2 = { x: 0, y: terrainVertHeight(x, z + 1), z: 0 + TERR_TRI_W };
            p3 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(x + 1, z + 1), z: 0 + TERR_TRI_W };
        } else { //    /|
            p1 = { x: 0, y: terrainVertHeight(x, z), z: 0 };
            p2 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(x + 1, z + 1), z: 0 + TERR_TRI_W };
            p3 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(x + 1, z), z: 0 };
        }
        return getNormal(p1, p2, p3);
    }


    // A function that returns the collision response for a bounding box
    function collisionResponse(bbx, bby, bbz, bbWidth, bbHeight, bbDepth, resolution, getBlockSolidnessCallback) {
        // The variables to store the collision flags and the corrected positions
        let stopX = false;
        let stopY = false;
        let stopZ = false;

        let cx = bbx;
        let cy = bby;
        let cz = bbz;

        let dx = 0;
        let dy = 0;
        let dz = 0;

        let wasCollision = false;

        const EPSILON = 0.1;

        // The variables to store the minimum and maximum coordinates of the bounding box
        let minX = bbx - bbWidth / 2 - EPSILON;
        let maxX = bbx + bbWidth / 2 + EPSILON;

        let minY = bby - bbHeight / 2 - EPSILON;
        let maxY = bby + bbHeight / 2 + EPSILON;

        let minZ = bbz - bbDepth / 2 - EPSILON;
        let maxZ = bbz + bbDepth / 2 + EPSILON;

        // The variables to store the minimum and maximum indices of the blocks that intersect with the bounding box
        let minBlockX = Math.floor(minX / resolution);
        let maxBlockX = Math.ceil(maxX / resolution);

        let minBlockY = Math.floor(minY / resolution);
        let maxBlockY = Math.ceil(maxY / resolution);

        let minBlockZ = Math.floor(minZ / resolution);
        let maxBlockZ = Math.ceil(maxZ / resolution);

        // Loop through all the blocks that intersect with the bounding box
        for (let i = minBlockX; i < maxBlockX; i++) {
            for (let j = minBlockY; j < maxBlockY; j++) {
                for (let k = minBlockZ; k < maxBlockZ; k++) {
                    // Check if the block is solid
                    if (getBlockSolidnessCallback(i, j, k)) {
                        wasCollision = true;
                        // Calculate the overlap between the bounding box and the block on each axis
                        let overlapX = Math.min(maxX, (i + 1) * resolution) - Math.max(minX, i * resolution);
                        let overlapY = Math.min(maxY, (j + 1) * resolution) - Math.max(minY, j * resolution);
                        let overlapZ = Math.min(maxZ, (k + 1) * resolution) - Math.max(minZ, k * resolution);

                        // Find the axis with the smallest overlap
                        let minOverlap = Math.min(overlapX, overlapY, overlapZ);

                        // Correct the position and set the collision flag on that axis
                        if (minOverlap === overlapX) {
                            if (bbx > i * resolution + resolution / 2) {
                                cx += overlapX;
                                dx += overlapX;
                            } else {
                                cx -= overlapX;
                                dx -= overlapX;
                            }
                            stopX = true;
                        } else if (minOverlap === overlapY) {
                            if (bby > j * resolution + resolution / 2) {
                                cy += overlapY;
                                dy += overlapY;
                            } else {
                                cy -= overlapY;
                                dy -= overlapY;
                            }
                            stopY = true;
                        } else if (minOverlap === overlapZ) {
                            if (bbz > k * resolution + resolution / 2) {
                                cz += overlapZ;
                                dz += overlapZ;
                            } else {
                                cz -= overlapZ;
                                dz -= overlapZ;
                            }
                            stopZ = true;
                        }
                    }
                }
            }
        }

        // Return the collision response as an object
        return { cx: cx, cy: cy, cz: cz, stopX: stopX, stopY: stopY, stopZ: stopZ, dx, dy, dz, wasCollision };
        // we only want to correct one axis at a time
        // so only set the axis that has the smallest overlap to non zero
        // the other axes will be zero

    }


    function getVoxelColliderAABBResponse(collider, modelMatrix, x, y, z) {
        // reverse transform the point by the model matrix
        let inverseModelMatrix = mat4.inverse(modelMatrix);
        let transformedPoint = multMat4Vec4(inverseModelMatrix, [x, y, z, 1]);
        // console.log('transformedPoint: ', transformedPoint);


        const isSolid = (xIndex, yIndex, zIndex) => {
            if (xIndex < 0 || xIndex >= collider.xNum || yIndex < 0 || yIndex >= collider.yNum || zIndex < 0 || zIndex >= collider.zNum)
                return false;
            return collider.voxels[xIndex + yIndex * collider.xNum + zIndex * collider.xNum * collider.yNum] == 1;
        };


        const response = collisionResponse(transformedPoint[0] - collider.xMin, transformedPoint[1] - collider.yMin, transformedPoint[2] - collider.zMin, 100, 600, 100, collider.resolution, isSolid);

        // transform the response back to world space using the model matrix
        const transformedResponse = multMat4Vec4(modelMatrix, [response.cx, response.cy, response.cz, 1]);

        // transform the response delta back to world space using the model matrix
        const transformedResponseDelta = multMat4Vec4(modelMatrix, [response.dx, response.dy, response.dz, 0]);

        response.cx = transformedResponse[0];
        response.cy = transformedResponse[1];
        response.cz = transformedResponse[2];

        response.dx = transformedResponseDelta[0];
        response.dy = transformedResponseDelta[1];
        response.dz = transformedResponseDelta[2];

        // return the response
        return response;

    }

    function collideWithEntity(entity, chunkWorldX, chunkWorldZ) {
        const rockModel = models[entity.type];

        const rockX = entity.x + chunkWorldX
        const rockY = entity.y + models[entity.type].offsetY;
        const rockZ = entity.z + chunkWorldZ;

        const rockDistanceToPlayer = Math.sqrt((rockX - playerX) * (rockX - playerX) + (rockZ - playerZ) * (rockZ - playerZ));

        if (rockDistanceToPlayer > 5000) return;


        if (!rockModel.collider || !entity.modelMatrix) return;


        const newPos = getVoxelColliderAABBResponse(rockModel.collider, entity.modelMatrix, playerX, playerY, playerZ);
        if (newPos.wasCollision)
            console.log('collision');

        // continue;
        // detect abnormally large dx, dy or dz (> ABNORMAL_D_SIZE)
        // then log player position and the dx, dy, dz
        const ABNORMAL_D_SIZE = 40;
        if (Math.abs(newPos.dx) > ABNORMAL_D_SIZE || Math.abs(newPos.dy) > ABNORMAL_D_SIZE || Math.abs(newPos.dz) > ABNORMAL_D_SIZE) {
            console.log('abnormal dx, dy, dz: ', newPos);
            console.log('player position: ', playerX, playerY, playerZ);
        }

        playerX += newPos.dx / 2;
        playerY += newPos.dy / 2;
        playerZ += newPos.dz / 2;

        if (Math.abs(newPos.dy) > 0.1) {
            playerYvel = 0;
            return true;
        }

        return false;

    }


    function tick() {
        let ms = 0.8;
        underwater = false;

        fcount += 0.25;
        tod += 0.000002;

        if (localStorage.fastTOD)
            tod += 0.000025;

        if (tod >= 1)
            tod = 0;

        if (flyMode) {
            if (keyDown.w) {
                playerXvel += -ms * Math.sin(playerAH);
                playerZvel += -ms * Math.cos(playerAH);
            } else if (keyDown.s) {
                playerXvel += ms * Math.sin(playerAH);
                playerZvel += ms * Math.cos(playerAH);
            }

            if (keyDown.a) {
                playerXvel += ms * Math.sin(playerAH - Math.PI / 2);
                playerZvel += ms * Math.cos(playerAH - Math.PI / 2);
            } else if (keyDown.d) {
                playerXvel += ms * Math.sin(playerAH + Math.PI / 2);
                playerZvel += ms * Math.cos(playerAH + Math.PI / 2);
            }

            if (keyDown.space) {
                playerYvel += ms;
            } else if (keyDown.shift) {
                playerYvel += -ms;
            }

            playerXvel *= 0.99;
            playerYvel *= 0.99;
            playerZvel *= 0.99;
        }

        planeRotVel.x *= 0.98;
        planeRotVel.y *= 0.98;
        planeRotVel.z *= 0.98;

        if (keyDown.up) {
            planeRotVel.x += 0.003;
        }
        if (keyDown.down) {
            planeRotVel.x += -0.003;
        }
        if (keyDown.left) {
            planeRotVel.y -= 0.003;
        }
        if (keyDown.right) {
            planeRotVel.y += 0.003;
        }
        if (keyDown.x) {
            planeVel.x += 1 * Math.sin(planeRot.y);
            planeVel.y += -1 * Math.sin(planeRot.x);
            planeVel.z += 1 * Math.cos(planeRot.y);
        }
        if (keyDown.z) {
        }

        if (playerAV < -Math.PI / 2) playerAV = -Math.PI / 2;
        else if (playerAV > Math.PI / 2) playerAV = Math.PI / 2;

        // if (keyDown.space) {
        //     playerYvel += ms;
        // } else if (keyDown.shift) {
        //     playerYvel += -ms;
        // }

        let frictionFact = 0.96;

        playerX += playerXvel;
        playerY += playerYvel;
        playerZ += playerZvel;

        headHeight += (600 - headHeight) / 10;

        planePos.x += planeVel.x;
        planePos.y += planeVel.y;
        planePos.z += planeVel.z;

        planeRot.x += planeRotVel.x;
        planeRot.y += planeRotVel.y;
        planeRot.z += planeRotVel.z;

        // keep the player within 5000 to 395000 in the x and z axis
        if (playerX < 5000) {
            playerX = 5000;
            playerXvel = 0;
        } else if (playerX > mapSize * TERR_TRI_W - 5000) {
            playerX = mapSize * TERR_TRI_W - 5000;
            playerXvel = 0;
        }

        if (playerZ < 5000) {
            playerZ = 5000;
            playerZvel = 0;
        } else if (playerZ > mapSize * TERR_TRI_W - 5000) {
            playerZ = mapSize * TERR_TRI_W - 5000;
            playerZvel = 0;
        }

        // chunks
        // if (playerX >= 1600) {
        //     updateCC();
        // } else if (playerX < 0) {
        //     if (pwx == 0) {
        //         playerX = 0;
        //         playerXvel = 0;
        //     } else {
        //         updateCC();
        //     }
        // }

        if (Math.abs(playerX - lastCCX) > 3200 || Math.abs(playerZ - lastCCZ) > 3200) {
            lastCCX = playerX;
            lastCCZ = playerZ;
            updateCC();
        }

        // if (playerZ >= 1600) {
        //     updateCC();
        // } else if (playerZ < 0) {
        //     if (pwz == 0) {
        //         playerZ = 0;
        //         playerZvel = 0;
        //     } else {
        //         updateCC();
        //     }
        // }

        playerYvel *= 0.999;

        playerXvel *= 0.999;
        playerZvel *= 0.999;
        playerHBV *= 0.9;

        planeVel.x *= 0.99;
        planeVel.y *= 0.99;
        planeVel.z *= 0.99;

        let absPlayerPos = getPlayerPos();
        let gm = false;
        let collMeshA = getTerrainHeight(absPlayerPos.x, absPlayerPos.z, true);
        let collHeight = collMeshA.h;
        let steepness = 1 - dotV(collMeshA.n, { x: 0, y: 1, z: 0 });

        if (playerY <= collHeight) { // coll Height
            playerY = collHeight;
            playerYvel = 0;
            gm = true;

            if (steepness > 0.3) {
                const pv = normalizeV({ x: collMeshA.n.x, y: 0, z: collMeshA.n.z });
                const pfact = (steepness - 0.3) * 1.8;
                playerXvel += pv.x * pfact;
                playerZvel += pv.z * pfact;
                frictionFact = 0.98;
            }
        } else if (playerY < collHeight + 160) {
            gm = true;
        }


        // COLLISION WITH OBJECTS!


        for (let i = 0; i < loadedChunks.length; i++) {
            const chunk = loadedChunks[i];
            const chunkWorldX = (chunk.chunkX) * chunkSize * TERR_TRI_W;
            const chunkWorldZ = (chunk.chunkZ) * chunkSize * TERR_TRI_W;
            const chunkDX = playerX - chunkWorldX;
            const chunkDZ = playerZ - chunkWorldZ;
            const distanceToPlayer = Math.sqrt(chunkDX * chunkDX + chunkDZ * chunkDZ);

            const renderDistance = chunkRenderRadius * chunkSize * TERR_TRI_W;

            if (renderDistance && distanceToPlayer > renderDistance) continue;

            for (let t = 0; t < chunk.rocks.length; t++) {
                if (collideWithEntity(chunk.rocks[t], chunkWorldX, chunkWorldZ)) gm = true;
            }

            for (let t = 0; t < chunk.trees.length; t++) {
                if (collideWithEntity(chunk.trees[t], chunkWorldX, chunkWorldZ)) gm = true;
            }

        }





        if (playerY <= -headHeight) {
            fogColor = zenithColor = [0.3 * ambientColor[0], 0.45 * ambientColor[1], 0.65 * ambientColor[2]];
            underwater = true;
        }

        if (playerY <= 0) {
            const wms = 0.1;

            if (keyDown.space && gameInput) {

                let md = (0 - playerY) / 450;
                if (md > 0.1) md = 0.1;
                if (md < 0) md = 0;
                playerYvel += md + 0.02;
            }

            playerXvel *= 0.991;
            playerYvel *= 0.991;
            playerZvel *= 0.991;

            playerYvel += Math.min(0.0205, (0 - playerY) / 10000);

            if (gameInput) {

                if (keyDown.w) {
                    playerXvel += -wms * Math.sin(playerAH);
                    playerYvel += wms * Math.sin(playerAV);
                    playerZvel += -wms * Math.cos(playerAH);
                } else if (keyDown.s) {
                    playerXvel += wms * Math.sin(playerAH);
                    playerZvel += wms * Math.cos(playerAH);
                }

                if (keyDown.a) {
                    playerXvel += wms * Math.sin(playerAH - Math.PI / 2);
                    playerZvel += wms * Math.cos(playerAH - Math.PI / 2);
                }

                if (keyDown.d) {
                    playerXvel += wms * Math.sin(playerAH + Math.PI / 2);
                    playerZvel += wms * Math.cos(playerAH + Math.PI / 2);
                }
            }

            playerYvel -= 0.02;
        } else {
            if (!flyMode)
                playerYvel -= 0.16;
        }

        if (gm) {
            playerXvel *= frictionFact;
            playerZvel *= frictionFact;
            if (gameInput) {
                if (keyDown.space && (Date.now() - ljumpTime) > 500 && steepness < 0.42) {
                    jumpcount = 0;
                    ljumpTime = Date.now();
                    ms = 12;
                    playerYvel += 19 / (1 + steepness * 1.2);
                }

                if (keyDown.shift) {
                    ms *= 1.93 * (stm * 0.4 + 0.6);
                    stm *= 0.998;
                } else {
                    stm += 0.0003;
                    if (stm > 1)
                        stm = 1;
                }

                ms *= clamp((1 - frictionFact) * 10, 0, 1);
                if (keyDown.w) {
                    playerHBV += 2;
                    playerXvel += -ms * Math.sin(playerAH);
                    playerZvel += -ms * Math.cos(playerAH);
                } else if (keyDown.s) {
                    playerHBV += 2;
                    playerXvel += ms * Math.sin(playerAH);
                    playerZvel += ms * Math.cos(playerAH);
                }

                if (keyDown.a) {
                    playerHBV += 2;
                    playerXvel += ms * Math.sin(playerAH - Math.PI / 2);
                    playerZvel += ms * Math.cos(playerAH - Math.PI / 2);
                }

                if (keyDown.d) {
                    playerHBV += 2;
                    playerXvel += ms * Math.sin(playerAH + Math.PI / 2);
                    playerZvel += ms * Math.cos(playerAH + Math.PI / 2);
                }
            }
        }
    }

    let ticksAc = 0;

    function createBuffer(w, h, isFP) {
        const colorTexture = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, colorTexture);
        isFP = useFPBuffers;
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, w, h, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
        gl.texImage2D(gl.TEXTURE_2D, 0, isFP ? gl.RGBA16F : gl.RGBA, w, h, 0, gl.RGBA, isFP ? gl.FLOAT : gl.UNSIGNED_BYTE, null);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);

        const depthTexture = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, depthTexture);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.DEPTH_COMPONENT24, w, h, 0, gl.DEPTH_COMPONENT, gl.UNSIGNED_INT, null);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

        const buffer = {
            fb: gl.createFramebuffer(),
            color: colorTexture,
            depth: depthTexture,
            w, h,
        };

        gl.bindFramebuffer(gl.FRAMEBUFFER, buffer.fb);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, colorTexture, 0);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.TEXTURE_2D, depthTexture, 0);

        return buffer;
    }

    // function renderCollider

    function renderTerrain(viewMatrix, cameraMatrix, clipHeight, clipAbove, clipEnabled, renderDistance) {
        gl.useProgram(terrainProg.prog);
        gl.uniformMatrix4fv(terrainProg.worldPositionLocation, false, mat4.translation(0, 0, 0));

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, textureGrass);
        gl.activeTexture(gl.TEXTURE1);
        gl.bindTexture(gl.TEXTURE_2D, textureRock);
        gl.activeTexture(gl.TEXTURE2);
        gl.bindTexture(gl.TEXTURE_2D, textureBeach);
        gl.activeTexture(gl.TEXTURE3);
        gl.bindTexture(gl.TEXTURE_2D, textureWaterDM);
        gl.activeTexture(gl.TEXTURE7);
        gl.bindTexture(gl.TEXTURE_2D, textureGrassNormal);
        gl.activeTexture(gl.TEXTURE8);
        gl.bindTexture(gl.TEXTURE_2D, textureRockNormal);
        gl.activeTexture(gl.TEXTURE9);
        gl.bindTexture(gl.TEXTURE_2D, textureBeachNormal);

        gl.enable(gl.DEPTH_TEST);
        gl.enable(gl.CULL_FACE);

        const pps = getPlayerPos();
        const lpps = getLocalSpace(pps.x, pps.y, pps.z);

        if (tod > 0.75 || tod < 0.25)
            flashlightColor = FLASHLIGHT_ON_COLOR;
        else
            flashlightColor = [0, 0, 0];

        gl.uniform3fv(terrainProg.pointLightPosLocation, [lpps.x + 0, lpps.y + 1000, lpps.z]); // positions
        gl.uniform3fv(terrainProg.pointLightColorLocation, flashlightColor); // colors
        gl.uniform1ui(terrainProg.numPointLightsLocation, lights.length); // number

        gl.uniform3fv(terrainProg.ambientLightColorLocation, ambientColor);

        if (clipEnabled) {
            gl.uniform1ui(terrainProg.clipAboveLocation, clipAbove);
            gl.uniform1f(terrainProg.clipHeightLocation, clipHeight);
        }

        gl.uniform1f(terrainProg.fcountLocation, fcount);
        gl.uniform1ui(terrainProg.clipEnabledLocation, clipEnabled);

        // dir light
        gl.uniform3fv(terrainProg.dirLightDirLocation, sunDir);
        gl.uniform3fv(terrainProg.dirLightColorLocation, skyDirColor);

        // cam pos
        gl.uniform3fv(terrainProg.viewWorldPositionLocation, cameraMatrix.slice(12, 15));
        gl.uniformMatrix4fv(terrainProg.projectionMatrixLocation, false, projectionMatrix);
        gl.uniformMatrix4fv(terrainProg.viewMatrixLocation, false, viewMatrix);

        // gl.uniform1f(terrainProg.fogDivisorLocation, fogDivisor);
        // gl.uniform3fv(terrainProg.fogColorLocation, fogColor);

        prevRenderedModel = null;

        // for all chunks
        for (let i = 0; i < loadedChunks.length; i++) {

            const chunk = loadedChunks[i];
            const chunkWorldX = (chunk.chunkX + 0.5) * chunkSize * TERR_TRI_W;
            const chunkWorldZ = (chunk.chunkZ + 0.5) * chunkSize * TERR_TRI_W;
            const chunkDX = playerX - chunkWorldX;
            const chunkDZ = playerZ - chunkWorldZ;
            const distanceToPlayer = Math.sqrt(chunkDX * chunkDX + chunkDZ * chunkDZ);

            if (renderDistance && distanceToPlayer > renderDistance) continue;

            gl.uniformMatrix4fv(terrainProg.worldPositionLocation, false, mat4.translation(chunk.chunkX * chunkSize * TERR_TRI_W, 0, chunk.chunkZ * chunkSize * TERR_TRI_W));

            // LOCATION, size, type, normalize, stride, offset
            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.buffer.positionBuffer);
            gl.vertexAttribPointer(terrainProg.positionAttributeLocation, 3, gl.FLOAT, false, 0, 0);
            gl.enableVertexAttribArray(terrainProg.positionAttributeLocation);

            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.buffer.brightnessBuffer);
            gl.vertexAttribPointer(terrainProg.brightnessAttributeLocation, 3, gl.FLOAT, false, 0, 0);
            gl.enableVertexAttribArray(terrainProg.brightnessAttributeLocation);

            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.buffer.normalBuffer);
            gl.vertexAttribPointer(terrainProg.normalLocation, 3, gl.FLOAT, false, 0, 0);
            gl.enableVertexAttribArray(terrainProg.normalLocation);

            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.buffer.texcoordBuffer);
            gl.vertexAttribPointer(terrainProg.texcoordAttributeLocation, 2, gl.FLOAT, true, 0, 0);
            gl.enableVertexAttribArray(terrainProg.texcoordAttributeLocation);

            gl.drawArrays(gl.TRIANGLES, 0, ((3 * 2 * chunkSize * chunkSize) >> (chunk.levelOfDetail - 1)) >> (chunk.levelOfDetail - 1));
        }

        for (let i = 0; i < loadedChunks.length; i++) {

            const chunk = loadedChunks[i];
            const chunkWorldX = (chunk.chunkX) * chunkSize * TERR_TRI_W;
            const chunkWorldZ = (chunk.chunkZ) * chunkSize * TERR_TRI_W;
            const chunkDX = playerX - chunkWorldX;
            const chunkDZ = playerZ - chunkWorldZ;
            const distanceToPlayer = Math.sqrt(chunkDX * chunkDX + chunkDZ * chunkDZ);

            if (renderDistance && distanceToPlayer > renderDistance) continue;

            // sort trees by type most importantly and then by distance to player
            chunk.trees.sort((a, b) => {
                if (a.type == b.type) {
                    const aDistanceToPlayer = Math.sqrt((a.x - playerX) * (a.x - playerX) + (a.z - playerZ) * (a.z - playerZ));
                    const bDistanceToPlayer = Math.sqrt((b.x - playerX) * (b.x - playerX) + (b.z - playerZ) * (b.z - playerZ));
                    return aDistanceToPlayer - bDistanceToPlayer;
                } else {
                    return a.type - b.type;
                }
            });

            for (let t = 0; t < chunk.trees.length; t++) {

                const treeModel = models[chunk.trees[t].type];

                const treeX = chunk.trees[t].x + chunkWorldX
                const treeY = chunk.trees[t].y + models[chunk.trees[t].type].offsetY;
                const treeZ = chunk.trees[t].z + chunkWorldZ + treeModel.scale * 5;

                const treeDistanceToPlayer = Math.sqrt((treeX - playerX) * (treeX - playerX) + (treeZ - playerZ) * (treeZ - playerZ));

                if (renderDistance && treeDistanceToPlayer > 80000) continue;

                const rotationMatrix = mat4.yRotation(chunk.trees[t].rotation);
                const scaleMatrix = mat4.scaling(chunk.trees[t].scale, chunk.trees[t].scale, chunk.trees[t].scale);
                const translationMatrix = mat4.translation(treeX, treeY, treeZ);
                chunk.trees[t].modelMatrix = mat4.multiply(translationMatrix, mat4.multiply(scaleMatrix, rotationMatrix));
                renderModel(treeModel, viewMatrix, cameraMatrix, chunk.trees[t].modelMatrix);

            }

            // sort rocks by type most importantly and then by distance to player
            chunk.rocks.sort((a, b) => {
                if (a.type == b.type) {
                    const aDistanceToPlayer = Math.sqrt((a.x - playerX) * (a.x - playerX) + (a.z - playerZ) * (a.z - playerZ));
                    const bDistanceToPlayer = Math.sqrt((b.x - playerX) * (b.x - playerX) + (b.z - playerZ) * (b.z - playerZ));
                    return aDistanceToPlayer - bDistanceToPlayer;
                } else {
                    return a.type - b.type;
                }
            });

            for (let t = 0; t < chunk.rocks.length; t++) {

                const rockModel = models[chunk.rocks[t].type];

                const rockX = chunk.rocks[t].x + chunkWorldX
                const rockY = chunk.rocks[t].y + models[chunk.rocks[t].type].offsetY;
                const rockZ = chunk.rocks[t].z + chunkWorldZ;

                const rockDistanceToPlayer = Math.sqrt((rockX - playerX) * (rockX - playerX) + (rockZ - playerZ) * (rockZ - playerZ));

                if (renderDistance && rockDistanceToPlayer > 80000) continue;

                const rotationMatrix = mat4.yRotation(chunk.rocks[t].rotation);
                const scaleMatrix = mat4.scaling(chunk.rocks[t].scale, chunk.rocks[t].scale, chunk.rocks[t].scale);
                const translationMatrix = mat4.translation(rockX, rockY, rockZ);
                chunk.rocks[t].modelMatrix = mat4.multiply(translationMatrix, mat4.multiply(scaleMatrix, rotationMatrix));
                renderModel(rockModel, viewMatrix, cameraMatrix, chunk.rocks[t].modelMatrix);
            }

            // sort underwater foliage by type most importantly and then by distance to player
            chunk.underwaterFoliage.sort((a, b) => {
                if (a.type == b.type) {
                    const aDistanceToPlayer = Math.sqrt((a.x - playerX) * (a.x - playerX) + (a.z - playerZ) * (a.z - playerZ));
                    const bDistanceToPlayer = Math.sqrt((b.x - playerX) * (b.x - playerX) + (b.z - playerZ) * (b.z - playerZ));
                    return aDistanceToPlayer - bDistanceToPlayer;
                } else {
                    return a.type - b.type;
                }
            });

            for (let t = 0; t < chunk.underwaterFoliage.length; t++) {

                const rockModel = models[chunk.underwaterFoliage[t].type];

                const rockX = chunk.underwaterFoliage[t].x + chunkWorldX
                const rockY = chunk.underwaterFoliage[t].y + models[chunk.underwaterFoliage[t].type].offsetY;
                const rockZ = chunk.underwaterFoliage[t].z + chunkWorldZ;

                const rockDistanceToPlayer = Math.sqrt((rockX - playerX) * (rockX - playerX) + (rockZ - playerZ) * (rockZ - playerZ));

                if (renderDistance && rockDistanceToPlayer > 80000) continue;

                const rotationMatrix = mat4.yRotation(chunk.underwaterFoliage[t].rotation);
                const scaleMatrix = mat4.scaling(chunk.underwaterFoliage[t].scale, chunk.underwaterFoliage[t].scale, chunk.underwaterFoliage[t].scale);
                const translationMatrix = mat4.translation(rockX, rockY, rockZ);
                renderModel(rockModel, viewMatrix, cameraMatrix, mat4.multiply(translationMatrix, mat4.multiply(scaleMatrix, rotationMatrix)));
            }
        }
    }

    function renderGrass(viewMatrix, cameraMatrix, clipHeight, clipAbove, clipEnabled, renderDistance) {
        gl.useProgram(grassProg.prog);
        gl.uniformMatrix4fv(grassProg.worldPositionLocation, false, mat4.translation(0, 0, 0));

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, textureGrassBB);

        gl.enable(gl.DEPTH_TEST);
        gl.disable(gl.CULL_FACE);

        // enable alpha blending
        // gl.enable(gl.BLEND);
        // gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

        gl.uniform3fv(grassProg.ambientLightColorLocation, ambientColor);

        if (clipEnabled) {
            gl.uniform1ui(grassProg.clipAboveLocation, clipAbove);
            gl.uniform1f(grassProg.clipHeightLocation, clipHeight);
        }

        gl.uniform1f(grassProg.fcountLocation, fcount);
        gl.uniform1ui(grassProg.clipEnabledLocation, clipEnabled);

        // dir light
        gl.uniform3fv(grassProg.dirLightDirLocation, sunDir);
        gl.uniform3fv(grassProg.dirLightColorLocation, skyDirColor);

        // flashlight
        const pps = getPlayerPos();
        const lpps = getLocalSpace(pps.x, pps.y, pps.z);

        gl.uniform3fv(grassProg.pointLightPosLocation, [lpps.x + 0, lpps.y + 1000, lpps.z]); // positions
        gl.uniform3fv(grassProg.pointLightColorLocation, flashlightColor); // colors

        // cam pos
        gl.uniformMatrix4fv(grassProg.projectionMatrixLocation, false, projectionMatrix);
        gl.uniformMatrix4fv(grassProg.viewMatrixLocation, false, viewMatrix);
        gl.uniform3fv(grassProg.viewWorldPositionLocation, cameraMatrix.slice(12, 15));

        let ppos = getLocalSpace(0, 0, 0);

        for (let i = 0; i < loadedChunks.length; i++) {
            const chunk = loadedChunks[i];
            const chunkWorldX = (chunk.chunkX + 0.5) * chunkSize * TERR_TRI_W;
            const chunkWorldZ = (chunk.chunkZ + 0.5) * chunkSize * TERR_TRI_W;
            const chunkDX = playerX - chunkWorldX;
            const chunkDZ = playerZ - chunkWorldZ;
            const distanceToPlayer = Math.sqrt(chunkDX * chunkDX + chunkDZ * chunkDZ);

            if (renderDistance && distanceToPlayer > renderDistance) continue;

            gl.uniformMatrix4fv(grassProg.worldPositionLocation, false, mat4.translation(chunk.chunkX * chunkSize * TERR_TRI_W, 0, chunk.chunkZ * chunkSize * TERR_TRI_W));

            // LOCATION, size, type, normalize, stride, offset
            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.grassBuffer.positionBuffer);
            gl.vertexAttribPointer(grassProg.positionAttributeLocation, 3, gl.FLOAT, false, 0, 0);
            gl.enableVertexAttribArray(grassProg.positionAttributeLocation);

            gl.bindBuffer(gl.ARRAY_BUFFER, chunk.grassBuffer.texcoordBuffer);
            gl.vertexAttribPointer(grassProg.texcoordAttributeLocation, 2, gl.FLOAT, true, 0, 0);
            gl.enableVertexAttribArray(grassProg.texcoordAttributeLocation);

            gl.drawArrays(gl.TRIANGLES, 0, chunk.grassBuffer.numTriangles * 3);
        }

        // disable alpha blending
        // gl.disable(gl.BLEND);
    }

    let prevRenderedModel = null;

    function renderModel(model, viewMatrix, cameraMatrix, modelMatrix, targetFB = null) {
        gl.useProgram(modelProg.prog);

        if (targetFB != null)
            gl.bindFramebuffer(gl.FRAMEBUFFER, targetFB);

        if (prevRenderedModel != model) {
            gl.activeTexture(gl.TEXTURE0);
            gl.bindTexture(gl.TEXTURE_2D, model.texture);

            gl.activeTexture(gl.TEXTURE7);
            gl.bindTexture(gl.TEXTURE_2D, model.textureNormal);
        }

        gl.enable(gl.DEPTH_TEST);
        model.backfaceCulling ? gl.enable(gl.CULL_FACE) : gl.disable(gl.CULL_FACE);

        gl.uniform3fv(modelProg.ambientLightColorLocation, ambientColor);
        gl.uniform1f(modelProg.fcountLocation, fcount);

        // dir light
        gl.uniform3fv(modelProg.dirLightDirLocation, sunDir);
        gl.uniform3fv(modelProg.dirLightColorLocation, skyDirColor);

        // flashlight
        const pps = getPlayerPos();
        const lpps = getLocalSpace(pps.x, pps.y, pps.z);

        gl.uniform3fv(modelProg.pointLightPosLocation, [lpps.x + 0, lpps.y + 1000, lpps.z]); // positions
        gl.uniform3fv(modelProg.pointLightColorLocation, flashlightColor); // colors

        // cam pos
        gl.uniformMatrix4fv(modelProg.projectionMatrixLocation, false, projectionMatrix);
        gl.uniformMatrix4fv(modelProg.viewMatrixLocation, false, viewMatrix);
        gl.uniform3fv(modelProg.viewWorldPositionLocation, cameraMatrix.slice(12, 15));

        gl.uniformMatrix4fv(modelProg.worldPositionLocation, false, modelMatrix);

        gl.bindBuffer(gl.ARRAY_BUFFER, model.positionBuffer);
        gl.vertexAttribPointer(modelProg.positionAttributeLocation, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(modelProg.positionAttributeLocation);

        gl.bindBuffer(gl.ARRAY_BUFFER, model.brightnessBuffer);
        gl.vertexAttribPointer(modelProg.brightnessAttributeLocation, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(modelProg.brightnessAttributeLocation);

        gl.bindBuffer(gl.ARRAY_BUFFER, model.normalBuffer);
        gl.vertexAttribPointer(modelProg.normalLocation, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(modelProg.normalLocation);

        gl.bindBuffer(gl.ARRAY_BUFFER, model.texcoordBuffer);
        gl.vertexAttribPointer(modelProg.texcoordAttributeLocation, 2, gl.FLOAT, true, 0, 0);
        gl.enableVertexAttribArray(modelProg.texcoordAttributeLocation);

        gl.drawArrays(gl.TRIANGLES, 0, model.positions.length / 3);
        prevRenderedModel = model;

    }

    function renderWater(viewMatrix, cameraMatrix, targetFB) {
        gl.bindFramebuffer(gl.FRAMEBUFFER, targetFB.fb);
        gl.useProgram(waterProg.prog);
        gl.disable(gl.CULL_FACE);
        gl.enable(gl.BLEND);
        gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

        gl.activeTexture(gl.TEXTURE4);
        gl.bindTexture(gl.TEXTURE_2D, refractionBuffer.color);
        gl.activeTexture(gl.TEXTURE5);
        gl.bindTexture(gl.TEXTURE_2D, refractionBuffer.depth);
        gl.activeTexture(gl.TEXTURE6);
        gl.bindTexture(gl.TEXTURE_2D, reflectionBuffer.color);

        //TODO: REMOVE!?
        gl.bindTexture(gl.TEXTURE_2D, reflectionBuffer.color);
        gl.activeTexture(gl.TEXTURE7);

        // water 
        let ppos = getLocalSpace(0, 0, 0);
        gl.uniformMatrix4fv(waterProg.worldPositionLocation, false, mat4.translation(~~(playerX / 3200) * 3200 - 245000, ppos.y, ~~(playerZ / 3200) * 3200 - 245000));
        // cam pos
        gl.uniform3fv(waterProg.viewWorldPositionLocation, cameraMatrix.slice(12, 15));
        gl.uniformMatrix4fv(waterProg.projectionMatrixLocation, false, projectionMatrix);
        gl.uniformMatrix4fv(waterProg.viewMatrixLocation, false, viewMatrix);

        gl.uniform3fv(waterProg.ambientLightColorLocation, ambientColor);

        gl.uniform3fv(waterProg.dirLightDirLocation, sunDir);

        gl.uniform1f(waterProg.fogDivisorLocation, fogDivisor);

        gl.uniform1f(waterProg.fcountLocation, fcount);
        gl.uniform3fv(waterProg.fogColorLocation, fogColor);

        gl.bindBuffer(gl.ARRAY_BUFFER, waterBuff.positionBuffer);
        gl.vertexAttribPointer(waterProg.positionAttributeLocation, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(waterProg.positionAttributeLocation);

        // gl.bindBuffer(gl.ARRAY_BUFFER, waterBuff.normalBuffer);
        // gl.vertexAttribPointer(waterProg.normalLocation, 3, gl.FLOAT, false, 0, 0);
        // gl.enableVertexAttribArray(waterProg.normalLocation);

        // gl.bindBuffer(gl.ARRAY_BUFFER, waterBuff.texcoordBuffer);
        // gl.vertexAttribPointer(waterProg.texcoordAttributeLocation, 2, gl.FLOAT, true, 0, 0);
        // gl.enableVertexAttribArray(waterProg.texcoordAttributeLocation);

        gl.drawArrays(gl.TRIANGLES, 0, 6);
        gl.disable(gl.BLEND);
    }

    function postProcessFinalPass(sourceBuffer, shader, targetFB) {
        gl.bindFramebuffer(gl.FRAMEBUFFER, targetFB);
        gl.useProgram(shader.prog);
        gl.disable(gl.CULL_FACE);
        gl.disable(gl.BLEND);

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, sourceBuffer.color);
        gl.activeTexture(gl.TEXTURE1);
        gl.bindTexture(gl.TEXTURE_2D, sourceBuffer.depth);

        gl.uniform1i(shader.imgInLocation, 0);
        gl.uniform1i(shader.imgDepthInLocation, 1);

        gl.bindBuffer(gl.ARRAY_BUFFER, fullScreenQuadBuff.positionBuffer);
        gl.vertexAttribPointer(shader.positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(shader.positionAttributeLocation);

        gl.drawArrays(gl.TRIANGLES, 0, 6);
    }

    function postProcessFogPass(sourceBuffer, shader, targetFB, useFogColor) {
        gl.bindFramebuffer(gl.FRAMEBUFFER, targetFB);
        gl.useProgram(shader.prog);
        gl.disable(gl.CULL_FACE);
        gl.disable(gl.BLEND);

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, sourceBuffer.color);
        gl.activeTexture(gl.TEXTURE1);
        gl.bindTexture(gl.TEXTURE_2D, sourceBuffer.depth);
        gl.activeTexture(gl.TEXTURE2);
        gl.bindTexture(gl.TEXTURE_2D, atmosphereFramebuffer.color);

        gl.uniform1i(shader.imgInLocation, 0);
        gl.uniform1i(shader.imgDepthInLocation, 1);
        gl.uniform1i(shader.imgAtmosphereInLocation, 2);

        gl.uniform1f(shader.fogDivisorLocation, fogDivisor);
        gl.uniform3fv(shader.fogColorLocation, fogColor);

        gl.uniform1ui(shader.useFogColorLocation, useFogColor ? 1 : 0);

        gl.bindBuffer(gl.ARRAY_BUFFER, fullScreenQuadBuff.positionBuffer);
        gl.vertexAttribPointer(shader.positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(shader.positionAttributeLocation);

        gl.drawArrays(gl.TRIANGLES, 0, 6);
    }

    // the only attribute that the atmosphere shader uses is the position
    function renderAtmosphere(viewMatrix, cameraMatrix, targetBuffer, renderSkyObjects) {
        gl.bindFramebuffer(gl.FRAMEBUFFER, targetBuffer);
        gl.useProgram(atmosphereProg.prog);
        gl.disable(gl.CULL_FACE);

        // atmosphere
        let ppos = getLocalSpace(0, 0, 0);
        gl.uniformMatrix4fv(atmosphereProg.worldPositionLocation, false, mat4.translation(~~(playerX / 3200) * 3200 - ATMSIZE / 2, ~~(playerY / 3200) * 3200 - ATMSIZE / 2, ~~(playerZ / 3200) * 3200 - ATMSIZE / 2));
        // cam pos
        gl.uniform3fv(atmosphereProg.viewWorldPositionLocation, cameraMatrix.slice(12, 15));
        gl.uniformMatrix4fv(atmosphereProg.projectionMatrixLocation, false, projectionMatrix);
        gl.uniformMatrix4fv(atmosphereProg.viewMatrixLocation, false, viewMatrix);

        gl.uniform1ui(atmosphereProg.renderSkyObjectsLocation, renderSkyObjects && true);

        // send the dir light dir uniform
        gl.uniform3fv(atmosphereProg.dirLightDirLocation, sunDir);

        // send the sun color
        gl.uniform3fv(atmosphereProg.sunColorLocation, skyDirColor);

        // send the zenith and horizon colors
        gl.uniform3fv(atmosphereProg.zenithColorLocation, zenithColor);
        gl.uniform3fv(atmosphereProg.horizonColorLocation, horizonColor);

        gl.bindBuffer(gl.ARRAY_BUFFER, atmosphereBuff.positionBuffer);
        gl.vertexAttribPointer(atmosphereProg.positionAttributeLocation, 3, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(atmosphereProg.positionAttributeLocation);

        // draw the correct number of vertices
        gl.drawArrays(gl.TRIANGLES, 0, atmosphereBuff.numTriangles * 3);
    }

    function renderCheckpoint(checkpointClipSpacePos, targetBuffer) {
        gl.bindFramebuffer(gl.FRAMEBUFFER, targetBuffer);
        gl.useProgram(imageProg.prog);
        gl.disable(gl.CULL_FACE);
        gl.disable(gl.DEPTH_TEST);
        gl.enable(gl.BLEND);
        gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, textureUICheckpoint);

        gl.uniform2fv(imageProg.imagePosLocation, [checkpointClipSpacePos.x, checkpointClipSpacePos.y]);
        gl.uniform2fv(imageProg.imageViewportResLocation, [gl.canvas.width, gl.canvas.height]);
        gl.uniform1ui(imageProg.imageTextureLocation, 0);

        gl.bindBuffer(gl.ARRAY_BUFFER, imageBuff.positionBuffer);
        gl.vertexAttribPointer(imageProg.positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);
        gl.enableVertexAttribArray(imageProg.positionAttributeLocation);

        // draw the correct number of vertices
        gl.drawArrays(gl.TRIANGLES, 0, 3 * 2);
        gl.disable(gl.BLEND);
    }

    function copyFB(width, height, fboIn, fboOut) {
        gl.bindFramebuffer(gl.READ_FRAMEBUFFER, fboIn);
        gl.bindFramebuffer(gl.DRAW_FRAMEBUFFER, fboOut);

        gl.blitFramebuffer(0, 0, width, height,
            0, 0, width, height,
            gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT, gl.NEAREST);
    }

    function normalizedSin(t) {
        return (Math.sin(t * Math.PI * 2) + 1) * 0.5;
    }

    function mix(c1, c2, t) {
        return [
            c1[0] * (1 - t) + c2[0] * t,
            c1[1] * (1 - t) + c2[1] * t,
            c1[2] * (1 - t) + c2[2] * t,
        ];
    }

    function clamp(n, min, max) {
        return Math.min(Math.max(n, min), max);
    }

    function clampVector(v, min, max) {
        return [
            clamp(v[0], min, max),
            clamp(v[1], min, max),
            clamp(v[2], min, max),
        ];
    }

    function multVecByScalar(v, s) {
        return [
            v[0] * s,
            v[1] * s,
            v[2] * s,
        ];
    }

    function dot(a, b) {
        return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
    }

    function normalize(v) {
        const l = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
        return [v[0] / l, v[1] / l, v[2] / l];
    }

    function normalizeV(v) {
        const l = Math.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
        return {
            x: v.x / l,
            y: v.y / l,
            z: v.z / l,
        };
    }

    function dotV(a, b) {
        return a.x * b.x + a.y * b.y + a.z * b.z;
    }

    function renderTrees(model, viewMatrix, cameraMatrix, modelMatrix, targetFB) {

    }

    function renderScene(viewMatrix, cameraMatrix, targetBuffer, options) {
        // bind the target buffer
        if (targetBuffer == null) {
            targetBuffer = {
                fb: null,
                w: gl.canvas.width,
                h: gl.canvas.height,
            }
        }
        gl.bindFramebuffer(gl.FRAMEBUFFER, targetBuffer.fb);
        gl.viewport(0, 0, targetBuffer.w, targetBuffer.h);

        renderTerrain(viewMatrix, cameraMatrix, options.clipHeight || 0, options.clipAbove || false, options.clipEnabled || false, options.renderDistance || 0);

        if (GRASS_ENABLED)
            renderGrass(viewMatrix, cameraMatrix, options.clipHeight || 0, options.clipAbove || false, options.clipEnabled || false, options.renderDistance || 0);

        // renderModel(models[0], viewMatrix, cameraMatrix, modelMatrix, targetBuffer.fb);

        gl.depthMask(false);
        renderAtmosphere(viewMatrix, cameraMatrix, targetBuffer.fb, true);
        gl.depthMask(true);
    }

    // Draw the scene.
    function drawScene(frameTimeNew) {
        if (frameTimeNew < 6000) {
            requestAnimationFrame(drawScene);
            return;
        }

        if (!counterStarted) {
            if (playerXvel != 0 || playerZvel != 0) {
                counterStarted = true;
                counterStart = performance.now();
            }

            document.getElementById('counter').innerText = '00:00.0';
        } else {
            const dt = (performance.now() - counterStart) / 1000;
            let mm = (~~(dt / 60));
            if (mm < 10)
                mm = '0' + mm;

            let ss = (~~((dt % 60) * 10)) / 10;
            if (ss < 10)
                ss = '0' + ss;

            document.getElementById('counter').innerText = mm + ':' + ss;
        }

        if (counterStarted && performance.now() - lastABSMovementRecord >= 100) {
            movementRecord.push({ type: 'abs', x: playerX, y: playerY, z: playerZ, h: playerAH, v: playerAV, t: performance.now() });
            lastABSMovementRecord = performance.now();
        }

        deltaTime = frameTimeNew - frameTime;
        frameTime = frameTimeNew;
        const pFactor = deltaTime / (16.666666666666666 / 2.4578);

        const elevation = Math.abs(dot([0, 1, 0], normalize(sunDir)));
        const elevation_6 = Math.pow(elevation, 0.6);
        const elevation8 = clamp(Math.pow(clamp(dot([0, 1, 0], normalize(sunDir)) + 0.5, 0, 1), 0.9) * 1.01, 0, 1);

        skyDirColor = multVecByScalar(mix([0.99, 0.4, 0.1], [0.9999, 0.92, 0.89], elevation), elevation8);
        tstval = stm;

        zenithColor = mix([0.650, 0.586, 0.870], [0.160, 0.305, 0.929], clamp(elevation_6, 0, 1));
        const brFact = clamp(((Math.cos((tod + 0.5) * 2 * Math.PI) + 1) / 2 + 0.05) * ((skyDirColor[0] + skyDirColor[1] + skyDirColor[2]) / 3) * 0.9, 0.01, 1);
        zenithColor[0] *= brFact;
        zenithColor[1] *= brFact;
        zenithColor[2] *= brFact;

        // horizon color
        horizonColor = mix([0.964, 0.85, 0.500], [0.984, 0.652, 0.156], clamp(1 - elevation_6, 0, 1));
        horizonColor[0] *= brFact;
        horizonColor[1] *= brFact;
        horizonColor[2] *= brFact;

        ambientColor = [zenithColor[0] * 0.2 + 0.006, zenithColor[1] * 0.2 + 0.006, zenithColor[2] * 0.2 + 0.008];
        fogColor = [zenithColor[0], zenithColor[1], zenithColor[2]]; // ew ik
        ticksAc += pFactor;

        if (ticksAc > 8)
            ticksAc = 8;

        for (let i = 0; i < ticksAc; i++)
            tick();

        ticksAc -= ~~ticksAc;

        const playerEyePos = {
            x: playerX,
            y: playerY + Math.sin(new Date().getTime() / 60) * playerHBV / 2 + headHeight,
            z: playerZ,
        };

        sunDir = mat4.normalize([Math.sin((tod + 0.5) * 2 * Math.PI), Math.cos((tod + 0.5) * 2 * Math.PI), 0.8]);

        playerAR = Math.sin(new Date().getTime() / 120) * playerHBV * 0.0004;



        let playerCameraMatrix = mat4.translation(playerEyePos.x, playerEyePos.y, playerEyePos.z);
        playerCameraMatrix = mat4.yRotate(playerCameraMatrix, playerAH);
        playerCameraMatrix = mat4.xRotate(playerCameraMatrix, playerAV);
        playerCameraMatrix = mat4.zRotate(playerCameraMatrix, playerAR);
        let playerViewMatrix = mat4.inverse(playerCameraMatrix);

        let reflectionCameraMatrix = mat4.translation(playerEyePos.x, 0 - playerEyePos.y, playerEyePos.z);
        reflectionCameraMatrix = mat4.yRotate(reflectionCameraMatrix, playerAH);
        reflectionCameraMatrix = mat4.xRotate(reflectionCameraMatrix, -playerAV);
        reflectionCameraMatrix = mat4.zRotate(reflectionCameraMatrix, -playerAR);
        let reflectionViewMatrix = mat4.inverse(reflectionCameraMatrix);



        // Checkpoint projection
        let m = projectionMatrix;
        m = mat4.multiply(m.slice(0), playerViewMatrix);
        m = mat4.translate(m, checkpoints[checkpointsReached].x, checkpoints[checkpointsReached].y + 600, checkpoints[checkpointsReached].z);

        const v = {
            x: 0,
            y: 0,
            z: 0,
            w: 1,
        }

        let u = [];
        u.x = v.x * m[0] + v.y * m[4] + v.z * m[8] + v.w * m[12];
        u.y = 0.0 + (v.x * m[1] + v.y * m[5] + v.z * m[9] + v.w * m[13]);
        u.z = v.x * m[2] + v.y * m[6] + v.z * m[10] + v.w * m[14];
        u.w = v.x * m[3] + v.y * m[7] + v.z * m[11] + v.w * m[15];

        u.x /= u.w;
        u.y /= u.w;
        u.z /= u.w;

        if (u.w < 0) {
            u.x = -2;
            u.y = -2;
        }

        // Checkpoint update
        if (Math.sqrt((playerX - checkpoints[checkpointsReached].x) ** 2 + (playerY - checkpoints[checkpointsReached].y) ** 2 + (playerZ - checkpoints[checkpointsReached].z) ** 2) < 2000) {
            if (checkpoints[checkpointsReached].newTOD)
                tod = checkpoints[checkpointsReached].newTOD;

            checkpointsReached++;
            if (checkpointsReached == checkpoints.length) {
                checkpointsReached = 0;
                alert('All reached');
            }
        }



        const atmosphericFogDivisor = 220000;

        gl.bindFramebuffer(gl.FRAMEBUFFER, refractionBuffer.fb);
        gl.viewport(0, 0, refractionBuffer.w, refractionBuffer.h);
        gl.clearColor(1, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        gl.bindFramebuffer(gl.FRAMEBUFFER, reflectionBuffer.fb);
        gl.viewport(0, 0, reflectionBuffer.w, reflectionBuffer.h);
        gl.clearColor(0, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        gl.bindFramebuffer(gl.FRAMEBUFFER, reflectionBuffer2.fb);
        gl.viewport(0, 0, reflectionBuffer2.w, reflectionBuffer2.h);
        gl.clearColor(0, 0, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        gl.bindFramebuffer(gl.FRAMEBUFFER, renderBuffer1.fb);
        gl.viewport(0, 0, renderBuffer1.w, renderBuffer1.h);
        gl.clearColor(0, 1, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        gl.bindFramebuffer(gl.FRAMEBUFFER, renderBuffer2.fb);
        gl.viewport(0, 0, renderBuffer2.w, renderBuffer2.h);
        gl.clearColor(0, 1, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        gl.bindFramebuffer(gl.FRAMEBUFFER, atmosphereFramebuffer.fb);
        gl.viewport(0, 0, atmosphereFramebuffer.w, atmosphereFramebuffer.h);
        gl.clearColor(0, 0, 1, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        gl.bindFramebuffer(gl.FRAMEBUFFER, null);
        gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
        gl.clearColor(1, 1, 0, 1);
        gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);

        fogDivisor = atmosphericFogDivisor;
        renderScene(playerViewMatrix, playerCameraMatrix, refractionBuffer, {
            renderDistance: chunkRenderRadius * chunkSize * TERR_TRI_W,
            clipHeight: WATER_CLIP_LEVEL + (underwater ? -10 : 10),
            clipAbove: true ^ (underwater),
            clipEnabled: false,
        });

        fogDivisor = underwater ? 16000 : atmosphericFogDivisor;
        renderScene(reflectionViewMatrix, reflectionCameraMatrix, reflectionBuffer2, {
            renderDistance: chunkRenderRadius * chunkSize * TERR_TRI_W,
            clipHeight: WATER_CLIP_LEVEL - (underwater ? -10 : 10),
            clipAbove: false ^ (underwater),
            clipEnabled: true,
        });

        fogDivisor = underwater ? 7000 : atmosphericFogDivisor;
        renderAtmosphere(playerViewMatrix, playerCameraMatrix, atmosphereFramebuffer.fb, false);
        postProcessFogPass(reflectionBuffer2, postProcFogProg, reflectionBuffer.fb, underwater);

        // No longer a requirement to perform the refraction render pass
        // TODO: fix the reflection edge artifacts caused by removing this pass and its clipping
        // renderScene(playerViewMatrix, playerCameraMatrix, renderBuffer1, {
        //     renderDistance: chunkRenderRadius * chunkSize * TERR_TRI_W,
        //     clipHeight: WATER_CLIP_LEVEL - (underwater ? -500 : 500),
        //     clipAbove: true ^ (playerEyePos.y > WATER_CLIP_LEVEL),
        //     clipEnabled: true,
        // });


        fogDivisor = underwater ? 99999999 : 10000;
        copyFB(refractionBuffer.w, refractionBuffer.h, refractionBuffer.fb, renderBuffer1.fb);

        renderWater(playerViewMatrix, playerCameraMatrix, renderBuffer1);

        fogDivisor = underwater ? 7000 : atmosphericFogDivisor;
        postProcessFogPass(renderBuffer1, postProcFogProg, renderBuffer2.fb, underwater);

        postProcessFinalPass(renderBuffer2, postProcFinalProg, null);

        // renderCheckpoint(u, null);

        if (!init)
            init = true;

        framesDrawn++;

        if (framesDrawn >= 150 || framesDrawn < 0) {
            fpsAverage = Math.round((framesDrawn / (performance.now() - fpsAvStartTime)) * 1000 * 100) / 100;
            fpsAvStartTime = performance.now();
            framesDrawn = 0;
        }

        requestAnimationFrame(drawScene);
    }


    function cubeData(size) {
        let positions = [
            // Top face (012023) // 0 // 1 // 2 // 3
            0, size, 0,
            0, size, size,
            size, size, size,
            0, size, 0,
            size, size, size,
            size, size, 0,

            // Bottom face
            0, 0, 0, // 4
            size, 0, 0, // 5
            size, 0, size, // 6
            0, 0, 0, // 4
            size, 0, size, // 6
            0, 0, size, // 7

            // front
            0, 0, size, // 8
            size, 0, size, // 9
            size, size, size, // 10
            0, 0, size, // 8
            size, size, size, // 10
            0, size, size, // 11size, size, size, // 10

            // Back face
            0, 0, 0, // 12
            0, size, 0, // 13
            size, size, 0, // 14
            0, 0, 0, // 12
            size, size, 0, // 14
            size, 0, 0, // 15

            // Right face
            size, 0, 0, // 16
            size, size, 0, // 17
            size, size, size, // 18
            size, 0, 0, // 16
            size, size, size, // 18
            size, 0, size, // 19

            // Left face
            0, 0, 0, // 20
            0, 0, size, // 21
            0, size, size, // 22
            0, 0, 0, // 20
            0, size, size, // 22
            0, size, 0, // 23
        ];

        // 0, 1, 2, 0, 2, 3,    // front
        //     4, 5, 6, 4, 6, 7,    // back

        //     8, 9, 10, 8, 10, 11,   // top
        //     12, 13, 14, 12, 14, 15,   // bottom

        //     16, 17, 18, 16, 18, 19,   // right
        //     20, 21, 22, 20, 22, 23,   // left

        let normals = [
            // top
            0, 1, 0,
            0, 1, 0,
            0, 1, 0,
            0, 1, 0,
            0, 1, 0,
            0, 1, 0,

            // bottom
            0, -1, 0,
            0, -1, 0,
            0, -1, 0,
            0, -1, 0,
            0, -1, 0,
            0, -1, 0,

            // front
            0, 0, 1,
            0, 0, 1,
            0, 0, 1,
            0, 0, 1,
            0, 0, 1,
            0, 0, 1,

            // Back face
            0, 0, -1,
            0, 0, -1,
            0, 0, -1,
            0, 0, -1,
            0, 0, -1,
            0, 0, -1,

            // Right face
            1, 0, 0,
            1, 0, 0,
            1, 0, 0,
            1, 0, 0,
            1, 0, 0,
            1, 0, 0,

            // Left face
            -1, 0, 0,
            -1, 0, 0,
            -1, 0, 0,
            -1, 0, 0,
            -1, 0, 0,
            -1, 0, 0,
        ];

        let brightnesses = multValF32(36, 0.2);

        let texcoords = [
            // Top
            0.0, 0.0,
            1.0, 0.0,
            1.0, 1.0,
        ];
        return { positions, normals, brightnesses, texcoords };
    }


    canvas.onclick = function (e) {
        canvas.requestPointerLock = canvas.requestPointerLock ||
            canvas.mozRequestPointerLock;
        document.exitPointerLock = document.exitPointerLock ||
            document.mozExitPointerLock;

        canvas.requestPointerLock();

        document.body.requestFullscreen();
    }

    canvas.addEventListener('mousedown', e => {
        if (e.button === 0) {
            leftMouseDown = true;
        } else {
            rightMouseDown = true;
        }
    });

    canvas.addEventListener('mouseup', e => {
        if (e.button === 0) {
            leftMouseDown = false;
        } else {
            rightMouseDown = false;
        }
    });

    document.addEventListener('pointerlockchange', lockChangeAlert, false);
    document.addEventListener('mozpointerlockchange', lockChangeAlert, false);

    function lockChangeAlert() {
        if (document.pointerLockElement === canvas ||
            document.mozPointerLockElement === canvas) {
            console.log('The pointer lock status is now locked');
            document.addEventListener("mousemove", updatePosition, false);
        } else {
            console.log('The pointer lock status is now unlocked');
            document.removeEventListener("mousemove", updatePosition, false);
        }
    }


    document.addEventListener('keydown', (e) => {
        // e.preventDefault();
        switch (e.keyCode) {
            case 87:
                movementRecord.push({ type: 'key', key: 'w', state: true, t: performance.now() });
                keyDown.w = true;
                break;
            case 65:
                movementRecord.push({ type: 'key', key: 'a', state: true, t: performance.now() });
                keyDown.a = true;
                break;
            case 83:
                movementRecord.push({ type: 'key', key: 's', state: true, t: performance.now() });
                keyDown.s = true;
                break;
            case 68:
                movementRecord.push({ type: 'key', key: 'd', state: true, t: performance.now() });
                keyDown.d = true;
                break;
            case 32:
                movementRecord.push({ type: 'key', key: ' ', state: true, t: performance.now() });
                keyDown.space = true;
                break;

            case 38:
                keyDown.up = true;
                break;
            case 39:
                keyDown.right = true;
                break;
            case 40:
                keyDown.down = true;
                break;
            case 37:
                keyDown.left = true;
                break;
            case 16:
                movementRecord.push({ type: 'key', key: '%S', state: true, t: performance.now() });
                keyDown.shift = true;
                break;
            case 17:
                keyDown.ctrl = true;
                e.preventDefault();
                break;
            case 88:
                keyDown.x = true;
                break;
            case 90:
                keyDown.z = true;
                break;
            case 49:
                lightMoveSelection = 0;
                break;
            case 50:
                lightMoveSelection = 1;
                break;
            case 51:
                lightMoveSelection = 2;
                break;

        }
    });

    document.addEventListener('keyup', (e) => {
        switch (e.keyCode) {
            case 87:
                movementRecord.push({ type: 'key', key: 'w', state: false, t: performance.now() });
                keyDown.w = false;
                break;
            case 65:
                movementRecord.push({ type: 'key', key: 'a', state: false, t: performance.now() });
                keyDown.a = false;
                break;
            case 83:
                movementRecord.push({ type: 'key', key: 's', state: false, t: performance.now() });
                keyDown.s = false;
                break;
            case 68:
                movementRecord.push({ type: 'key', key: 'd', state: false, t: performance.now() });
                keyDown.d = false;
                break;
            case 32:
                movementRecord.push({ type: 'key', key: ' ', state: false, t: performance.now() });
                keyDown.space = false;
                break;
            case 191:
            case 13:
                if (Date.now() - latestCommandEnterTime > 200)
                    focusCommandPanel();
                break;

            case 38:
                keyDown.up = false;
                break;
            case 39:
                keyDown.right = false;
                break;
            case 40:
                keyDown.down = false;
                break;
            case 37:
                keyDown.left = false;
                break;
            case 16:
                movementRecord.push({ type: 'key', key: '%s', state: false, t: performance.now() });
                keyDown.shift = false;
                break;
            case 17:
                keyDown.ctrl = false;
                break;
            case 88:
                keyDown.x = false;
                break;
            case 90:
                keyDown.z = false;
                break;
        }
    });

    document.addEventListener('touchstart', (e) => {
        ltx = e.touches[0].clientX;
        lty = e.touches[0].clientY;
        if (ltx < window.innerWidth / 2) {
            keyDown.w = true;
            keyDown.ctrl = true;
        }
    });

    document.addEventListener('touchend', (e) => {
        keyDown.w = false;
    });

    canvas.addEventListener('touchmove', (e) => {
        e.preventDefault();
        playerAH += (ltx - e.touches[0].clientX) / 80;
        playerAV += (lty - e.touches[0].clientY) / 80;
        ltx = e.touches[0].clientX;
        lty = e.touches[0].clientY;
    });

    function updatePosition(e) {
        playerAH -= e.movementX / 2000 * sensitivity;
        playerAV -= e.movementY / 2000 * sensitivity;
        playerAH = playerAH % 6.28;
        playerAV = playerAV % 6.28;
    }

    function createShaderProgram(vertexSource, fragSource) {
        let error = [];
        function compileShader(gl, type, source) {
            let shader = gl.createShader(type);
            gl.shaderSource(shader, source);
            gl.compileShader(shader);
            let success = gl.getShaderParameter(shader, gl.COMPILE_STATUS);
            if (success) {
                return shader;
            }

            let e = gl.getShaderInfoLog(shader);

            // console.log(e, source);
            // instead log the faulty shader with line numbers then tab for each line so we can debug
            console.log(e);
            let lines = source.split('\n');
            for (let i = 0; i < lines.length; i++) {
                console.log(i + 1 + '\t' + lines[i]);
            }



            error[error.length] = e;
            gl.deleteShader(shader);
        }

        function createProgram(gl, vertexShader, fragmentShader) {
            let program = gl.createProgram();
            gl.attachShader(program, vertexShader);
            gl.attachShader(program, fragmentShader);
            gl.linkProgram(program);
            let success = gl.getProgramParameter(program, gl.LINK_STATUS);
            if (success) {
                return program;
            }

            let e = gl.getProgramInfoLog(program);
            console.log(e);
            error[error.length] = e;
            gl.deleteProgram(program);
        }
        let vertexShader;
        let fragmentShader;
        let prog;
        try {
            vertexShader = compileShader(gl, gl.VERTEX_SHADER, vertexSource);
            fragmentShader = compileShader(gl, gl.FRAGMENT_SHADER, fragSource);
            prog = createProgram(gl, vertexShader, fragmentShader);
        } catch (err) { };

        return {
            prog: prog,
            error: error
        }
    }

    function sizeCanvas() {
        canvas.width = resolution.w * DISP_SCALE;
        canvas.height = resolution.h * DISP_SCALE;
        gl.viewport(0, 0, canvas.width, canvas.height);
    }

    setInterval(() => {
        let ah = Math.floor(radToDeg(playerAH));
        let av = Math.floor(radToDeg(playerAV));
        let px = Math.floor(playerX);
        let py = Math.floor(playerY);
        let pz = Math.floor(playerZ);
        document.getElementById('stats').innerHTML = 'X:' + px + '<br>Y:' + py + '<br>Z:' + pz + '<br>FPS: ' + fpsAverage + '<br>Time (0-1): ' + (~~(tod * 1000) / 1000);
    }, 100);

    function genHeightMap() {
        let seed = 2378929;////34234;////3341; // 3345;
        let gen = new PRNG(seed);
        let perm = new Array(512);
        let gradP = new Array(512);
        let grad3 = [new Grad(1, 1, 0), new Grad(-1, 1, 0), new Grad(1, -1, 0), new Grad(-1, -1, 0),
        new Grad(1, 0, 1), new Grad(-1, 0, 1), new Grad(1, 0, -1), new Grad(-1, 0, -1),
        new Grad(0, 1, 1), new Grad(0, -1, 1), new Grad(0, 1, -1), new Grad(0, -1, -1)];

        function Grad(x, y, z) {
            this.x = x; this.y = y; this.z = z;
        }

        Grad.prototype.dot2 = function (x, y) {
            return this.x * x + this.y * y;
        };

        let F2 = 0.5 * (Math.sqrt(3) - 1);
        let G2 = (3 - Math.sqrt(3)) / 6;

        let F3 = 1 / 3;
        let G3 = 1 / 6;

        seedGen();

        function seedGen(seed) {
            for (let i = 0; i < 256; i++) {
                let v = Math.floor(gen.get() * 255);

                perm[i] = perm[i + 256] = v;
                gradP[i] = gradP[i + 256] = grad3[v % 12];
            }
        };

        function simplex(xin, yin) {
            let n0, n1, n2;
            let s = (xin + yin) * F2;
            let i = Math.floor(xin + s);
            let j = Math.floor(yin + s);
            let t = (i + j) * G2;
            let x0 = xin - i + t;
            let y0 = yin - j + t;
            let i1, j1;
            if (x0 > y0) {
                i1 = 1; j1 = 0;
            } else {
                i1 = 0; j1 = 1;
            }
            let x1 = x0 - i1 + G2;
            let y1 = y0 - j1 + G2;
            let x2 = x0 - 1 + 2 * G2;
            let y2 = y0 - 1 + 2 * G2;
            i &= 255;
            j &= 255;
            let gi0 = gradP[i + perm[j]];
            let gi1 = gradP[i + i1 + perm[j + j1]];
            let gi2 = gradP[i + 1 + perm[j + 1]];
            let t0 = 0.5 - x0 * x0 - y0 * y0;
            if (t0 < 0) {
                n0 = 0;
            } else {
                t0 *= t0;
                n0 = t0 * t0 * gi0.dot2(x0, y0);
            }
            let t1 = 0.5 - x1 * x1 - y1 * y1;
            if (t1 < 0) {
                n1 = 0;
            } else {
                t1 *= t1;
                n1 = t1 * t1 * gi1.dot2(x1, y1);
            }
            let t2 = 0.5 - x2 * x2 - y2 * y2;
            if (t2 < 0) {
                n2 = 0;
            } else {
                t2 *= t2;
                n2 = t2 * t2 * gi2.dot2(x2, y2);
            }
            return 70 * (n0 + n1 + n2);
        };

        for (let y = 0; y < mapSize; y++) {
            for (let x = 0; x < mapSize; x++) {
                const vx = (mapSize * TERR_TRI_W / 2 - 207853) / 400;
                const vy = (mapSize * TERR_TRI_W / 2 - 203332) / 400;
                const dx = (x - 150000 / 400 - vx) / 1.3;
                const dy = (y - 150000 / 400 - vy) / 1.3;
                // let mult = simplex((x - vx) / 200, (y - vy) / 200) + 1;
                // map[y * mapSize + x] = simplex(dx / 598, dy / 596) * 1 * 0.5;
                // map[y * mapSize + x] = simplex(dx / 382, dy / 382) * 1 * 0.5;
                // map[y * mapSize + x] += simplex(dx / 182, dy / 182) * mult * 0.4;
                // map[y * mapSize + x] += simplex(dx / 1082, dy / 1082) * mult * 0.4;
                // map[y * mapSize + x] += simplex(dx / 4095, dy / 4093) * mult * 0.8;
                // map[y * mapSize + x] += simplex(dx / 32, dy / 32) * mult * 0.4;
                // map[y * mapSize + x] += simplex(dx / 5, dy / 5) * 0.02 + 0.1;


                // const areaHillyness = simplex(dx / 1100, dy / 1100) * 0.8 + 0.2;
                let areaHeight = simplex(dx / 400, dy / 400) * 1.8 + 1.9;
                areaHeight += simplex(dx / 200 - 200, dy / 200) * 1.0;
                areaHeight += simplex(dx / 100 - 100, dy / 100) * 0.5;
                areaHeight += simplex(dx / 50 - 300, dy / 50) * 0.2;
                areaHeight += simplex(dx / 20 - 300, dy / 20) * 0.1;
                areaHeight += simplex(dx / 10 - 400, dy / 10) * 0.03;
                areaHeight += simplex(dx / 5, dy / 5) * 0.01;

                map[y * mapSize + x] = areaHeight;
            }
        }

        loadingSP.innerText += "Eroding terrain...\n";

        erodeTerrain(1000);

        loadingSP.innerText += "Loading textures...\n";
    }

    function erodeTerrain(iterations) {
        return; // incomplete

        const seed = 2389451;
        const gen = new PRNG(seed);
        // Water-related constants
        const rain_rate = 0.0008 * 0.75;
        const evaporation_rate = 0.0005;

        // Slope constants
        const min_height_delta = 0.05;
        const repose_slope = 0.03;
        const gravity = 30.0;
        const gradient_sigma = 0.5;

        // Sediment constants
        const sediment_capacity_constant = 50.0;
        const dissolving_rate = 0.25;
        const deposition_rate = 0.001;

        const water = new Float32Array(mapSize * mapSize);
        const sediment = new Float32Array(mapSize * mapSize);
        const velocity = new Float32Array(mapSize * mapSize);

        function getMeshCoord(x, z) {
            let gx = Math.floor(x); // get quad x
            let gz = Math.floor(z); // get quad z
            let s = x - gx < z - gz; // true if triangle with edge on mx = 0, mx = 1 ...; false if triangle edge on line gz = 0, gz = 1 ...
            return [gx, gz, s];
        }

        function terrainVertHeight(x, z) {
            return map[z * mapSize + x];
        }

        function getTerrainHeight(x, z) { // get the interpolated height for a coord on the terrain map
            function interpolateTriangle(
                p1 /*vec3*/,
                p2 /*vec3*/,
                p3 /*vec3*/,
                pos /*vec2*/
            ) {
                let det = (p2.z - p3.z) * (p1.x - p3.x) + (p3.x - p2.x) * (p1.z - p3.z);
                let l1 = ((p2.z - p3.z) * (pos.x - p3.x) + (p3.x - p2.x) * (pos.y - p3.z)) / det;
                let l2 = ((p3.z - p1.z) * (pos.x - p3.x) + (p1.x - p3.x) * (pos.y - p3.z)) / det;
                let l3 = 1 - l1 - l2;
                return l1 * p1.y + l2 * p2.y + l3 * p3.y;
            }

            let meshCoord = getMeshCoord(x, z);
            let mcx = meshCoord[0];
            let mcz = meshCoord[1];
            let mcs = meshCoord[2];
            let p1, p2, p3;
            if (mcs) { // z = 0 edged triangle
                p1 = { x: 0, y: terrainVertHeight(meshCoord[0], meshCoord[1]), z: 0 };
                p2 = { x: 0, y: terrainVertHeight(meshCoord[0], meshCoord[1] + 1), z: 0 + TERR_TRI_W };
                p3 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(meshCoord[0] + 1, meshCoord[1] + 1), z: 0 + TERR_TRI_W };
            } else {
                p1 = { x: 0, y: terrainVertHeight(meshCoord[0], meshCoord[1]), z: 0 };
                p2 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(meshCoord[0] + 1, meshCoord[1] + 1), z: 0 + TERR_TRI_W };
                p3 = { x: 0 + TERR_TRI_W, y: terrainVertHeight(meshCoord[0] + 1, meshCoord[1]), z: 0 };
            }
            let pos = { x: x - mcx, y: z - mcz };
            return interpolateTriangle(p1, p2, p3, pos);
        }

        function select(cond, choice, def) {
            for (let i = 0; i < cond; i++) {
                if (cond[i])
                    return choice[i];
            }

            return def || 0;
        }

        const erode = () => {
            for (let y = 0; y < mapSize; y++) {
                for (let x = 0; x < mapSize; x++) {
                    const cellIndex = y * mapSize + x;

                    const indices = [
                        { dir: { x: 0, y: -1 } },
                        { dir: { x: 0, y: 1 } },
                        { dir: { x: -1, y: 0 } },
                        { dir: { x: 1, y: 0 } },
                        { dir: { x: -1, y: -1 } },
                        { dir: { x: -1, y: 1 } },
                        { dir: { x: 1, y: 1 } },
                        { dir: { x: 1, y: -1 } },
                    ];

                    let gradX = 0;
                    let gradY = 0;

                    for (let i = 0; i < indices.length; i++) {
                        const neighborIndex = indices[i].index = cellIndex + indices[i].dir.x + indices[i].dir.y * mapSize;
                        if (indices[i].index < 0 || indices[i].index >= mapSize * mapSize)
                            indices[i].index = 0;

                        const grad = indices[i].dh = map[cellIndex] - map[neighborIndex];
                        gradX += grad * indices[i].dir.x;
                        gradY += grad * indices[i].dir.y;
                    }

                    const gradAbs = Math.sqrt(gradX * gradX + gradY * gradY);
                    gradX /= gradAbs;
                    gradY /= gradAbs;

                    water[cellIndex] += gen.get() * rain_rate;

                    const neighborHeight = getTerrainHeight(x + gradX, y + gradY);
                    const heightDelta = map[cellIndex] - neighborHeight;

                    const sediment_capacity = (Math.max(heightDelta, min_height_delta) / 0.75) * velocity[cellIndex] * water[cellIndex] * sediment_capacity_constant;
                    let deposited_sediment = select(
                        [
                            height_delta < 0,
                            sediment > sediment_capacity,
                        ],
                        [
                            Math.min(heightDelta, sediment),
                            deposition_rate * (sediment - sediment_capacity),
                        ],
                        // If sediment <= sediment_capacity
                        dissolving_rate * (sediment - sediment_capacity)
                    );

                    deposited_sediment = Math.max(-heightDelta, deposited_sediment);
                    sediment[cellIndex] -= deposited_sediment;
                    map[cellIndex] += deposited_sediment;


                }
            }
        };

        for (let i = 0; i < iterations; i++)
            erode();
    }

    // this function increases the heightmap at the players position
    function incheight(r) {
        let x = ~~(playerX / TERR_TRI_W);
        let z = ~~(playerZ / TERR_TRI_W);

        // increase the map in a dome shape around x and z using the distance
        for (let dz = -r; dz <= r; dz++) {
            for (let dx = -r; dx <= r; dx++) {
                let dist = Math.sqrt(dx * dx + dz * dz);
                if (dist < r) {
                    map[x + dx + (z + dz) * mapSize] += 0.01 * (1 - Math.pow(dist / r, 0.8));
                }
            }
        }

        // only rebuild the chunks for the changed area
        let x1 = Math.max(0, x - r);
        let x2 = Math.min(mapSize, x + r);
        let z1 = Math.max(0, z - r);
        let z2 = Math.min(mapSize, z + r);
        let c = 0;
        for (let z = z1; z < z2; z += chunkSize) {
            for (let x = x1; x < x2; x += chunkSize) {
                rebuildChunk(~~(x / chunkSize), ~~(z / chunkSize));
                c++;
            }
        }

        console.log('rebuilt ' + c + ' chunks');

        // updateCC();
    }

    function decheight(r) {
        let x = ~~(playerX / TERR_TRI_W);
        let z = ~~(playerZ / TERR_TRI_W);

        // increase the map in a dome shape around x and z using the distance
        for (let dz = -r; dz <= r; dz++) {
            for (let dx = -r; dx <= r; dx++) {
                let dist = Math.sqrt(dx * dx + dz * dz);
                if (dist < r) {
                    map[x + dx + (z + dz) * mapSize] -= 0.06 * (1 - Math.pow(dist / r, 2));
                }
            }
        }

        updateCC();
    }

    function getLocalSpace(x, y, z) { // converts from large scale coords to local 3D space (for floating point accuracy issues)
        return {
            x: x - pwx * FP_CHUNK_SIZE,
            y: y - pwy * FP_CHUNK_SIZE,
            z: z - pwz * FP_CHUNK_SIZE,
        }
    }

    function getPlayerPos() {
        return {
            x: playerX + pwx * FP_CHUNK_SIZE,
            y: playerY + pwy * FP_CHUNK_SIZE,
            z: playerZ + pwz * FP_CHUNK_SIZE,
        }
    }

    function commandOutputPush(msg) {
        const co = document.getElementById('commandoutput');
        co.innerText += msg + '\n';
        co.scrollTop = co.scrollHeight;

    }

    commandOutputPush('Type "help" for a list of commands\n');


    function processCommand(cmd) {
        if (cmd[0] === '/')
            cmd = cmd.substring(1);
        if (cmd.length == '')
            return;
        document.getElementById('commandinput').value = '';

        let firstSpaceIndex = cmd.indexOf(' ');
        if (firstSpaceIndex < 0)
            firstSpaceIndex = cmd.length;

        const cmdArgs = cmd.slice(firstSpaceIndex + 1);
        switch (cmd.substring(0, firstSpaceIndex)) {
            case 'help': {
                commandOutputPush('Commands: [changelog, time, tp, screenshot, tod, fly, walk]\nType command name without arguments for command info.');
                break;
            }
            case 'changelog': {
                showScrollPanel(changelog);
                break;
            }
            case 'time': {
                switch (cmdArgs) {
                    case 'night':
                        tod = 0.9;
                        commandOutputPush('Time set to night');
                        hideCommandPanel();
                        break;
                    case 'day':
                        tod = 0.4;
                        commandOutputPush('Time set to day');
                        hideCommandPanel();
                        break;
                    case 'morning':
                        tod = 0.220;
                        commandOutputPush('Time set to morning');
                        hideCommandPanel();
                        break;
                    case 'evening':
                        tod = 0.720;
                        commandOutputPush('Time set to evening');
                        hideCommandPanel();
                        break;
                    default:
                        commandOutputPush('Possible values: [night, day, evening, morning]');
                }

                break;
            }
            case 'tp': {
                const args = cmdArgs.trim().split(' ');
                if (args.length != 3) {
                    commandOutputPush('Use format: tp <x> <y> <z>');
                    return;
                }

                const px = +args[0];
                const py = +args[1];
                const pz = +args[2];

                if (px === NaN || py === NaN || pz === NaN) {
                    commandOutputPush('Only numbers should be provided. Use format: tp <x> <y> <z>');
                    return;
                }

                playerX = px;
                playerY = py;
                playerZ = pz;

                commandOutputPush('Teleported to ' + px + ', ' + py + ', ' + pz);
                hideCommandPanel();

                break;
            }

            // command to download an image of the canvas
            case 'screenshot': {
                const canvas = gl.canvas;
                const link = document.createElement('a');
                link.download = 'screenshot.png';
                link.href = canvas.toDataURL();
                link.click();
                break;
            }

            // command to move the player to a random location within to 0-400000 square bounds
            case 'random': {
                playerX = Math.floor(Math.random() * 400000);
                playerZ = Math.floor(Math.random() * 400000);

                // set the player y to the height of the terrain at the player's x,z
                playerY = getTerrainHeight(playerX, playerZ);

                commandOutputPush('Teleported to ' + playerX + ', ' + playerY + ', ' + playerZ);
                hideCommandPanel();
                break;
            }

            // command to set time of day as a number between 0 and 1
            case 'tod': {
                const args = cmdArgs.trim().split(' ');
                if (args.length != 1) {
                    commandOutputPush('Use format: tod <number>');
                    return;
                }

                const todNum = +args[0];

                if (todNum === NaN) {
                    commandOutputPush('Only numbers should be provided. Use format: tod <number>');
                    return;
                }

                tod = todNum;

                commandOutputPush('Time of day set to ' + tod);
                hideCommandPanel();
                break;
            }

            // command to turn on fly mode
            case 'fly': {
                flyMode = true;
                commandOutputPush('Fly mode enabled');
                hideCommandPanel();
                break;
            }

            case 'walk': {
                flyMode = false;
                commandOutputPush('Fly mode disabled');
                hideCommandPanel();
                break;
            }


            default:
                commandOutputPush('Unknown command: ' + cmd);

        }
    }


    function showScrollPanel(text) {
        if (window.scrollPanelOpen)
            return;
        window.scrollPanelOpen = true;
        const scrollPanel = document.createElement('div');
        scrollPanel.classList.add('scrollpanel');
        scrollPanel.innerText = text;

        const scrollPanelExit = document.createElement('div');
        scrollPanelExit.innerText = 'Close';
        scrollPanelExit.classList.add('scrollpanelexit');

        scrollPanel.close = () => {
            scrollPanel.remove();
            scrollPanelExit.remove();
            window.scrollPanelOpen = false;
        };

        scrollPanelExit.addEventListener('click', scrollPanel.close);

        document.body.appendChild(scrollPanel);
        document.body.appendChild(scrollPanelExit);

        if (document.exitPointerLock)
            document.exitPointerLock();

        hideCommandPanel();

        return scrollPanel;
    }


    function PRNG(seed) {
        this.state = seed || 123456789;
        this.seed = (seed) => this.state = seed; // functions to set the seed
        this.get = _ => {
            let bits = this.state;
            bits ^= (bits >> 7); // XOR "bits" by bits with a 7 bit right shift
            bits ^= (bits << 9); // XOR "bits" by bits with a 9 bit left shift
            bits ^= (bits >> 13); // XOR "bits" by bits with a 13 bit right shift
            this.state = bits; // use the "random" outcome as the seed for the next number that is gotten with "get()"
            return this.state / (2 ** 31); // convert from 32-bit int to float from [0, 1) by divifing it by 2 raised to (Max int size + 1)
        };
    };

    function multValF32(num, val, arr = false) {
        let a = [];
        for (let i = 0; i < num; i++) {
            if (arr) {
                for (let p = 0; p < arr.length; p++) {
                    a[a.length] = val[p];
                }
            } else {
                a[i] = val;
            }
        }
        return Float32Array.from(a);
    }

    // window.onbeforeunload = function (e) {
    //     // Cancel the event
    //     e.preventDefault();

    //     e.returnValue = 'Are you sure you want to quit?';
    // };

    document.onkeydown = function (e) {
        e = e || window.event;

        if (!e.ctrlKey) return;

        let code = e.which || e.keyCode;

        switch (code) {
            case 83:
            case 87:
                e.preventDefault();
                e.stopPropagation();
                break;
        }
    };

    document.getElementById('commandinput').addEventListener('focus', focusCommandPanel);
    document.getElementById('commandinput').addEventListener('blur', hideCommandPanel);
    canvas.addEventListener('click', hideCommandPanel);

    document.getElementById('commandinput').addEventListener('keydown', e => {
        if (e.code != 'Enter')
            return;
        latestCommandEnterTime = Date.now();
        processCommand(e.currentTarget.value);
    });

    function focusCommandPanel() {
        gameInput = false;
        document.getElementById('commandinput').focus();
        document.getElementById('commandoutput').style.opacity = '1';
    }

    function hideCommandPanel() {
        gameInput = true;
        document.getElementById('commandinput').blur();
        document.getElementById('commandoutput').style.opacity = '0';
    }

    // the render distance buttons
    let renderDistBtns = [
        { id: 'renderhigh', rad: 12, el: null },
        { id: 'rendermed', rad: 8, el: null },
        { id: 'renderlow', rad: 5, el: null },
    ];

    let toneMappingButtons = [
        { id: 'tmA', a: 1, el: null },
        { id: 'tmB', a: 2, el: null },
        { id: 'tmC', a: 3, el: null },
    ];

    // add the listeners to the buttons and highlight the selected button with the "active" class
    // initially set it to high
    let renderDistBtnActive = renderDistBtns[0];

    function renderDistBtnClick(btn) {
        renderDistBtnActive.el.classList.remove('active');
        btn.el.classList.add('active');

        chunkRenderRadius = btn.rad;
        updateCC();
    }

    for (let i = 0; i < renderDistBtns.length; i++) {
        const btn = renderDistBtns[i];
        btn.el = document.getElementById(btn.id);
        btn.el.onclick = () => {
            renderDistBtnClick(btn);
            renderDistBtnActive = btn;
        };
    }

    renderDistBtnClick(renderDistBtnActive, renderDistBtnActive);

    let toneMappingBtnActive = toneMappingButtons[0];

    function toneMappingBtnClick(btn) {
        toneMappingBtnActive.el.classList.remove('active');
        btn.el.classList.add('active');

        gl.useProgram(postProcFinalProg.prog);
        gl.uniform1i(postProcFinalProg.toneMappingSelectionLocation, btn.a);
    }

    for (let i = 0; i < toneMappingButtons.length; i++) {
        const btn = toneMappingButtons[i];
        btn.el = document.getElementById(btn.id);
        btn.el.onclick = () => {
            toneMappingBtnClick(btn);
            toneMappingBtnActive = btn;
        };
    }

    toneMappingBtnClick(toneMappingBtnActive, toneMappingBtnActive);
}

function startInResolution(res) {
    begin(res, false, -1);
    document.getElementById('startpanel').style.display = 'none';

    async function music() {
        const context = new AudioContext();

        const response = await fetch('./2023_Mix.ogg');
        const arrayBuffer = await response.arrayBuffer();
        const audioBuffer = await context.decodeAudioData(arrayBuffer);

        play(audioBuffer);

        function play(audioBuffer) {
            const source = context.createBufferSource();
            source.buffer = audioBuffer;
            source.connect(context.destination);
            source.start();

            source.loop = true;
        }
    }

    // music();
}

const startResolutions = [
    { w: screen.width * window.devicePixelRatio, h: screen.height * window.devicePixelRatio, d: 'Best Quality', native: true, hdr: true, },
    { w: ~~(screen.width * window.devicePixelRatio * 0.75), h: ~~(screen.height * window.devicePixelRatio * 0.75), d: 'Medium Quality', hdr: true, },
    { w: ~~(screen.width * window.devicePixelRatio * 0.5), h: ~~(screen.height * window.devicePixelRatio * 0.5), d: 'Lowest Quality', hdr: true, },
];

// begin(startResolutions[0], true, -1);
// document.getElementById('startmenu').style.display = 'none';

for (let i = 0; i < startResolutions.length; i++) {
    const resolution = startResolutions[i];
    const resolutionOptionButton = document.createElement('div');
    resolutionOptionButton.classList.add('resolutionoption');

    if (resolution.link)
        resolutionOptionButton.innerText = resolution.d;
    else
        resolutionOptionButton.innerText = 'Start in ' + resolution.w + 'x' + resolution.h + (resolution.d ? (' (' + resolution.d + ')') : '');

    if (i === 0)
        resolutionOptionButton.classList.add('primary');

    if (resolution.link) {
        resolutionOptionButton.onclick = () => {
            window.open(resolution.link, '_blank');
        };
    } else {
        resolutionOptionButton.onclick = () => {
            startInResolution(resolution);
        };
    }
    document.getElementById('startoptions').appendChild(resolutionOptionButton);
}

function degToRad(deg) {
    return deg * Math.PI / 180;
}
