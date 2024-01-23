const mat4 = {
    perspective: function (fieldOfViewInRadians, aspect, near, far) {
        let f = Math.tan(Math.PI * 0.5 - 0.5 * fieldOfViewInRadians);
        let rangeInv = 1.0 / (near - far);

        return [
            f / aspect, 0, 0, 0,
            0, f, 0, 0,
            0, 0, (near + far) * rangeInv, -1,
            0, 0, near * far * rangeInv * 2, 0,
        ];
    },

    projection: function (width, height, depth) {
        return [
            2 / width, 0, 0, 0,
            0, -2 / height, 0, 0,
            0, 0, 2 / depth, 0,
            -1, 1, 0, 1,
        ];
    },

    multiply: function (a, b) {
        let a00 = a[0 * 4 + 0];
        let a01 = a[0 * 4 + 1];
        let a02 = a[0 * 4 + 2];
        let a03 = a[0 * 4 + 3];
        let a10 = a[1 * 4 + 0];
        let a11 = a[1 * 4 + 1];
        let a12 = a[1 * 4 + 2];
        let a13 = a[1 * 4 + 3];
        let a20 = a[2 * 4 + 0];
        let a21 = a[2 * 4 + 1];
        let a22 = a[2 * 4 + 2];
        let a23 = a[2 * 4 + 3];
        let a30 = a[3 * 4 + 0];
        let a31 = a[3 * 4 + 1];
        let a32 = a[3 * 4 + 2];
        let a33 = a[3 * 4 + 3];
        let b00 = b[0 * 4 + 0];
        let b01 = b[0 * 4 + 1];
        let b02 = b[0 * 4 + 2];
        let b03 = b[0 * 4 + 3];
        let b10 = b[1 * 4 + 0];
        let b11 = b[1 * 4 + 1];
        let b12 = b[1 * 4 + 2];
        let b13 = b[1 * 4 + 3];
        let b20 = b[2 * 4 + 0];
        let b21 = b[2 * 4 + 1];
        let b22 = b[2 * 4 + 2];
        let b23 = b[2 * 4 + 3];
        let b30 = b[3 * 4 + 0];
        let b31 = b[3 * 4 + 1];
        let b32 = b[3 * 4 + 2];
        let b33 = b[3 * 4 + 3];
        return [
            b00 * a00 + b01 * a10 + b02 * a20 + b03 * a30,
            b00 * a01 + b01 * a11 + b02 * a21 + b03 * a31,
            b00 * a02 + b01 * a12 + b02 * a22 + b03 * a32,
            b00 * a03 + b01 * a13 + b02 * a23 + b03 * a33,
            b10 * a00 + b11 * a10 + b12 * a20 + b13 * a30,
            b10 * a01 + b11 * a11 + b12 * a21 + b13 * a31,
            b10 * a02 + b11 * a12 + b12 * a22 + b13 * a32,
            b10 * a03 + b11 * a13 + b12 * a23 + b13 * a33,
            b20 * a00 + b21 * a10 + b22 * a20 + b23 * a30,
            b20 * a01 + b21 * a11 + b22 * a21 + b23 * a31,
            b20 * a02 + b21 * a12 + b22 * a22 + b23 * a32,
            b20 * a03 + b21 * a13 + b22 * a23 + b23 * a33,
            b30 * a00 + b31 * a10 + b32 * a20 + b33 * a30,
            b30 * a01 + b31 * a11 + b32 * a21 + b33 * a31,
            b30 * a02 + b31 * a12 + b32 * a22 + b33 * a32,
            b30 * a03 + b31 * a13 + b32 * a23 + b33 * a33,
        ];
    },

    translation: function (tx, ty, tz) {
        return [
            1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0,
            tx, ty, tz, 1,
        ];
    },

    xRotation: function (angleInRadians) {
        let c = Math.cos(angleInRadians);
        let s = Math.sin(angleInRadians);

        return [
            1, 0, 0, 0,
            0, c, s, 0,
            0, -s, c, 0,
            0, 0, 0, 1,
        ];
    },

    yRotation: function (angleInRadians) {
        let c = Math.cos(angleInRadians);
        let s = Math.sin(angleInRadians);

        return [
            c, 0, -s, 0,
            0, 1, 0, 0,
            s, 0, c, 0,
            0, 0, 0, 1,
        ];
    },

    zRotation: function (angleInRadians) {
        let c = Math.cos(angleInRadians);
        let s = Math.sin(angleInRadians);

        return [
            c, s, 0, 0,
            -s, c, 0, 0,
            0, 0, 1, 0,
            0, 0, 0, 1,
        ];
    },

    scaling: function (sx, sy, sz) {
        return [
            sx, 0, 0, 0,
            0, sy, 0, 0,
            0, 0, sz, 0,
            0, 0, 0, 1,
        ];
    },

    translate: function (m, tx, ty, tz) {
        return mat4.multiply(m, mat4.translation(tx, ty, tz));
    },

    xRotate: function (m, angleInRadians) {
        return mat4.multiply(m, mat4.xRotation(angleInRadians));
    },

    yRotate: function (m, angleInRadians) {
        return mat4.multiply(m, mat4.yRotation(angleInRadians));
    },

    zRotate: function (m, angleInRadians) {
        return mat4.multiply(m, mat4.zRotation(angleInRadians));
    },

    scale: function (m, sx, sy, sz) {
        return mat4.multiply(m, mat4.scaling(sx, sy, sz));
    },

    inverse: function (m) {
        let m00 = m[0 * 4 + 0];
        let m01 = m[0 * 4 + 1];
        let m02 = m[0 * 4 + 2];
        let m03 = m[0 * 4 + 3];
        let m10 = m[1 * 4 + 0];
        let m11 = m[1 * 4 + 1];
        let m12 = m[1 * 4 + 2];
        let m13 = m[1 * 4 + 3];
        let m20 = m[2 * 4 + 0];
        let m21 = m[2 * 4 + 1];
        let m22 = m[2 * 4 + 2];
        let m23 = m[2 * 4 + 3];
        let m30 = m[3 * 4 + 0];
        let m31 = m[3 * 4 + 1];
        let m32 = m[3 * 4 + 2];
        let m33 = m[3 * 4 + 3];
        let tp0 = m22 * m33;
        let tp1 = m32 * m23;
        let tp2 = m12 * m33;
        let tp3 = m32 * m13;
        let tp4 = m12 * m23;
        let tp5 = m22 * m13;
        let tp6 = m02 * m33;
        let tp7 = m32 * m03;
        let tp8 = m02 * m23;
        let tp9 = m22 * m03;
        let tp10 = m02 * m13;
        let tp11 = m12 * m03;
        let tp12 = m20 * m31;
        let tp13 = m30 * m21;
        let tp14 = m10 * m31;
        let tp15 = m30 * m11;
        let tp16 = m10 * m21;
        let tp17 = m20 * m11;
        let tp18 = m00 * m31;
        let tp19 = m30 * m01;
        let tp20 = m00 * m21;
        let tp21 = m20 * m01;
        let tp22 = m00 * m11;
        let tp23 = m10 * m01;

        let t0 = (tp0 * m11 + tp3 * m21 + tp4 * m31) -
            (tp1 * m11 + tp2 * m21 + tp5 * m31);
        let t1 = (tp1 * m01 + tp6 * m21 + tp9 * m31) -
            (tp0 * m01 + tp7 * m21 + tp8 * m31);
        let t2 = (tp2 * m01 + tp7 * m11 + tp10 * m31) -
            (tp3 * m01 + tp6 * m11 + tp11 * m31);
        let t3 = (tp5 * m01 + tp8 * m11 + tp11 * m21) -
            (tp4 * m01 + tp9 * m11 + tp10 * m21);

        let d = 1.0 / (m00 * t0 + m10 * t1 + m20 * t2 + m30 * t3);

        return [
            d * t0,
            d * t1,
            d * t2,
            d * t3,
            d * ((tp1 * m10 + tp2 * m20 + tp5 * m30) -
                (tp0 * m10 + tp3 * m20 + tp4 * m30)),
            d * ((tp0 * m00 + tp7 * m20 + tp8 * m30) -
                (tp1 * m00 + tp6 * m20 + tp9 * m30)),
            d * ((tp3 * m00 + tp6 * m10 + tp11 * m30) -
                (tp2 * m00 + tp7 * m10 + tp10 * m30)),
            d * ((tp4 * m00 + tp9 * m10 + tp10 * m20) -
                (tp5 * m00 + tp8 * m10 + tp11 * m20)),
            d * ((tp12 * m13 + tp15 * m23 + tp16 * m33) -
                (tp13 * m13 + tp14 * m23 + tp17 * m33)),
            d * ((tp13 * m03 + tp18 * m23 + tp21 * m33) -
                (tp12 * m03 + tp19 * m23 + tp20 * m33)),
            d * ((tp14 * m03 + tp19 * m13 + tp22 * m33) -
                (tp15 * m03 + tp18 * m13 + tp23 * m33)),
            d * ((tp17 * m03 + tp20 * m13 + tp23 * m23) -
                (tp16 * m03 + tp21 * m13 + tp22 * m23)),
            d * ((tp14 * m22 + tp17 * m32 + tp13 * m12) -
                (tp16 * m32 + tp12 * m12 + tp15 * m22)),
            d * ((tp20 * m32 + tp12 * m02 + tp19 * m22) -
                (tp18 * m22 + tp21 * m32 + tp13 * m02)),
            d * ((tp18 * m12 + tp23 * m32 + tp15 * m02) -
                (tp22 * m32 + tp14 * m02 + tp19 * m12)),
            d * ((tp22 * m22 + tp16 * m02 + tp21 * m12) -
                (tp20 * m12 + tp23 * m22 + tp17 * m02)),
        ];
    },

    cross: function (a, b) {
        return [
            a[1] * b[2] - a[2] * b[1],
            a[2] * b[0] - a[0] * b[2],
            a[0] * b[1] - a[1] * b[0],
        ];
    },

    subtractVectors: function (a, b) {
        return [a[0] - b[0], a[1] - b[1], a[2] - b[2]];
    },

    normalize: function (v) {
        let length = Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]);
        // make sure we don't divide by 0.
        if (length > 0.00001) {
            return [v[0] / length, v[1] / length, v[2] / length];
        } else {
            return [0, 0, 0];
        }
    },

    lookAt: function (cameraPosition, target, up) {
        let zAxis = mat4.normalize(
            mat4.subtractVectors(cameraPosition, target));
        let xAxis = mat4.normalize(mat4.cross(up, zAxis));
        let yAxis = mat4.normalize(mat4.cross(zAxis, xAxis));

        return [
            xAxis[0], xAxis[1], xAxis[2], 0,
            yAxis[0], yAxis[1], yAxis[2], 0,
            zAxis[0], zAxis[1], zAxis[2], 0,
            cameraPosition[0],
            cameraPosition[1],
            cameraPosition[2],
            1,
        ];
    },

    transformVector: function (m, v) {
        let dst = [];
        for (let i = 0; i < 4; ++i) {
            dst[i] = 0.0;
            for (let j = 0; j < 4; ++j) {
                dst[i] += v[j] * m[j * 4 + i];
            }
        }
        return dst;
    },

};

class Vec3 {
    constructor(x, y, z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
    set(x, y, z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
    clone() {
        return new Vec3(this.x, this.y, this.z);
    }
    add(b) {
        this.x += b.x;
        this.y += b.y;
        this.z += b.z;
        return this;
    }
    sub(b) {
        this.x -= b.x;
        this.y -= b.y;
        this.z -= b.z;
        return this;
    }
    mult(b) {
        this.x *= b.x;
        this.y *= b.y;
        this.z *= b.z;
        return this;
    }
    div(b) {
        this.x /= b.x;
        this.y /= b.y;
        this.z /= b.z;
        return this;
    }
    multScalar(s) {
        this.x *= s;
        this.y *= s;
        this.z *= s;
        return this;
    }
    divScalar(s) {
        this.x /= s;
        this.y /= s;
        this.z /= s;
        return this;
    }
    length() {
        return Math.sqrt(this.x * this.x + this.y * this.y + this.z * this.z);
    }
    normalize() {
        this.divScalar(this.length());
        return this;
    }
    square() {
        this.x *= this.x;
        this.y *= this.y;
        this.z *= this.z;
        return this;
    }
    dot(b) {
        return this.x * b.x + this.y * b.y + this.z * b.z;
    }
};

class VerletPoint {
    constructor(pos, mass, t) {
        this.pos = pos;
        this.prevPos = pos;
        this.force = new Vec3(0, 0, 0);
        this.mass = mass;
        this.t = t;
    }

    updatePosition() {
        const posTemp = this.pos.clone();
        this.pos.add(this.pos.clone().sub(this.prevPos)).add(this.force.multScalar(this.t * this.t / this.mass));
        this.prevPos = posTemp;
        this.force.set(0, 0, 0);
    }

    getVel() {
        return this.pos.clone().sub(this.prevPos).divScalar(this.t);
    }
}

class Quaternion {
    // JS quaternion class by Inriz
    // Useful info: https://en.wikipedia.org/wiki/Quaternion

    constructor(q) {
        if (q) {
            this.a = q.a;
            this.b = q.b;
            this.c = q.c;
            this.d = q.d;
        } else {
            this.a = 0;
            this.b = 0;
            this.c = 0;
            this.d = 0;
        }
    }

    clone() {
        return new Quaternion(this);
    }

    add(q) {
        this.a += q.a;
        this.b += q.b;
        this.c += q.c;
        this.d += q.d;

        return this;
    };

    subtract(q) {
        this.a -= q.a;
        this.b -= q.b;
        this.c -= q.c;
        this.d -= q.d;

        return this;
    };

    multiply(q) {
        const thisA = this.a;
        const thisB = this.b;
        const thisC = this.c;
        const thisD = this.d;

        this.a = thisA * q.a - thisB * q.b - thisC * q.c - thisD * q.d;
        this.b = thisA * q.b + thisB * q.a + thisC * q.d - thisD * q.c;
        this.c = thisA * q.c - thisB * q.d + thisC * q.a + thisD * q.b;
        this.d = thisA * q.d + thisB * q.c - thisC * q.b + thisD * q.a;

        return this;
    };

    multiplyScalar(x) {
        this.a *= x;
        this.b *= x;
        this.c *= x;
        this.d *= x;

        return this;
    };

    conjugate() {
        this.a = this.a;
        this.b = -this.b;
        this.c = -this.c;
        this.d = -this.d;

        return this;
    };

    reciprocal() {
        const r = 1 / (this.a * this.a + this.b * this.b
            + this.c * this.c + this.d * this.d);

        this.a = this.a * r;
        this.b = -this.b * r;
        this.c = -this.c * r;
        this.d = -this.d * r;

        return this;
    };

    length() {
        return Math.sqrt(this.a * this.a + this.b * this.b
            + this.c * this.c + this.d * this.d);
    };

    normalize() {
        const recipLen = 1 / this.length();
        return this.multiplyScalar(recipLen);
    };


    fromAxisAngle(axis, angle) {
        const r = 1 / Math.sqrt(axis.x * axis.x
            + axis.y * axis.y + axis.z * axis.z);
        const s = Math.sin(angle / 2);

        this.a = Math.cos(angle / 2);
        this.b = s * axis.x * r;
        this.c = s * axis.y * r;
        this.d = s * axis.z * r;

        return this;
    };

    getAngle() {
        if (this.a < -1 || this.a > 1)
            return 0;

        const angle = 2 * Math.acos(this.a);
        if (angle > Math.PI)
            return (angle - 2 * Math.PI);

        return angle;
    };

    getAxis() {
        const r = 1 / Math.sqrt(this.b * this.b + this.c * this.c + this.d * this.d);
        return {
            x: this.b * r,
            y: this.c * r,
            z: this.d * r,
        };
    };

    getMatrix4x4() {
        const _2bc = 2 * this.b * this.c;
        const _2cc = 2 * this.c * this.c;
        const _2dd = 2 * this.d * this.d;
        const _2ad = 2 * this.a * this.d;
        const _2bd = 2 * this.b * this.d;
        const _2ac = 2 * this.a * this.c;
        const _2bb = 2 * this.b * this.b;
        const _2cd = 2 * this.c * this.d;
        const _2ab = 2 * this.a * this.b;
        return [
            1 - _2cc - _2dd, _2bc - _2ad, _2bd + _2ac, 0,
            _2bc + _2ad, 1 - _2bb - _2dd, _2cd - _2ab, 0,
            _2bd - _2ac, _2cd + _2ab, 1 - _2bb - _2cc, 0,
            0, 0, 0, 1,
        ];
    }

    toString(decimalPlaces) {
        decimalPlaces = (decimalPlaces === null || decimalPlaces === undefined) ? 3 : decimalPlaces;
        const multiplier = Math.pow(10, decimalPlaces);
        const rAbs = Math.round(Math.abs(this.a) * multiplier) / multiplier;
        const iAbs = Math.round(Math.abs(this.b) * multiplier) / multiplier;
        const jAbs = Math.round(Math.abs(this.c) * multiplier) / multiplier;
        const kAbs = Math.round(Math.abs(this.d) * multiplier) / multiplier;
        const r = this.a == 0 ? '' : (this.a > 0 ? '' : ' - ') + rAbs;
        const i = this.b == 0 ? '' : (this.b > 0 ? ' + ' : ' - ') + iAbs + 'i';
        const j = this.c == 0 ? '' : (this.c > 0 ? ' + ' : ' - ') + jAbs + 'j';
        const k = this.d == 0 ? '' : (this.d > 0 ? ' + ' : ' - ') + kAbs + 'k';
        return r + i + j + k;
    }
};
