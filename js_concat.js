function println () {
    for (var i=0; i<arguments.length; ++i) {
        document.write(arguments[i]);
    }
    document.writeln();
}
function dir (obj) {
    for (var prop in obj) {
        document.writeln(prop
                         + (obj.hasOwnProperty(prop) ? '  (own):    ' : '  (inherited):    ')
                         + obj[prop]);
    }
}
Object.beget = function (obj) {
    var ChildConstr = function () {};
    ChildConstr.prototype = obj;
    return new ChildConstr();
};
Function.prototype.method = function (name, func) {
    this.prototype[name] = func;
    return this;
};

// Javascript Concatenation.
// 4. The Object Model of Javascript.
//-----------------------------------

var sin = Math.sin;
var cos = Math.cos;
var pi = Math.PI;

var Color = {
    'red'    : 'red',
    'green'  : 'green',
    'blue'   : 'blue',
    'yellow' : 'yellow',
    'black'  : 'black',
    'white'  : 'white'
};

// We can define a "pseudoclass" using a constructor function...
function Turtle(x, y, heading) {
    this.x = x;
    this.y = y;
    this.heading = heading;
}

// and make "instances"...
var t1 = new Turtle(100, 100, 0);
var t2 = new Turtle(100, 100, 45);

println(t2);
dir(t2);
println();

// We can add methods and instance vars to the pseudoclass using the prototype property...
Turtle.prototype.forward = function(dist) {
    this.x += dist * cos(this.heading * pi / 180);
    this.y += dist * sin(this.heading * pi / 180);
}
Turtle.prototype.rotate = function(angle) {
    this.heading += rotate;
}

println(t2);
dir(t2);
println();

t2.forward(100);
println(t2);
dir(t2);
println();

/*  In order to define a "superclass", we must..
    1. Define a constructor function which invokes the constructor function of the
       "superprototype" to initialise those properties inherited from the superclass; and
    2. Manually initialise those properties (instance variables) specific to the subclass.
    This process is known as "constructor chaining".
 */
function ColorTurtle(x, y, heading, color) {
    Turtle.call(this, x, y, heading);
    this.color = color;
}
ColorTurtle.prototype = new Turtle();
ColorTurtle.prototype.constructor = ColorTurtle;

ColorTurtle.prototype.setColor = function(col) {
    this.color = col;
}

var t3 = new ColorTurtle(100, 200, 0, Color.blue);

println(t3);
dir(t3);
println();

t3.forward(10);
t3.setColor(Color.green);

println(t3);
dir(t3);
println();
