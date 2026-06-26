hl.curve("overshoot", {
	type = "bezier",
	points = {
		{ 0.86, -0.69 },
		{ 0.1,  1.81 },
	}
})

hl.curve("rubber", {
	type = "spring",
	mass = 1,
	stiffness = 50,
	dampening = 5
})

hl.animation({
	leaf = "workspaces",
	enabled = true,
	speed = 8,
	bezier = "overshoot"
})

hl.animation({
	leaf = "windows",
	enabled = true,
	speed = 10,
	spring = "rubber",
	style = "slide"
})

hl.animation({
	leaf = "fade",
	enabled = true,
	speed = 0.5,
	spring = "rubber"
})

hl.animation({
	leaf = "layersIn",

	enabled = true,

	speed = 5,
	spring = "rubber",
	style = "popin 120%",
})

hl.animation({
	leaf = "layersOut",
	enabled = true,
	speed = 5,
	spring = "rubber",
	style = "popin 120%",
})
