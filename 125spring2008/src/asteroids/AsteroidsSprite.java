package asteroids;

/************************************************************************************************
 * The AsteroidsSprite class defines a game object, including it's shape, position, movement and
 * rotation. It also can detemine if two objects collide.
 ************************************************************************************************/

import java.awt.Color;
import java.awt.Polygon;

public class AsteroidsSprite {

	// Fields:

	static int width; // Dimensions of the graphics area.

	static int height;

	Polygon shape; // Initial sprite shape, centered at the origin (0,0).

	boolean active; // Active flag.

	double angle; // Current angle of rotation.

	double deltaAngle; // Amount to change the rotation angle.

	double currentX, currentY; // Current position on screen.

	double deltaX, deltaY; // Amount to change the screen position.

	Polygon sprite; // Final location and shape of sprite after applying

	// rotation and

	// moving to screen position. Used for drawing on the screen and
	// in detecting collisions.
	Color colour; // Colour of asteroid

	// Constructors:

	public AsteroidsSprite() {

		this.shape = new Polygon();
		this.active = false;
		this.angle = 0.0;
		this.deltaAngle = 0.0;
		this.currentX = 0.0;
		this.currentY = 0.0;
		this.deltaX = 0.0;
		this.deltaY = 0.0;
		this.sprite = new Polygon();
		this.colour = new Color(0, 0, 0);
	}

	// Methods:

	public void advance() {

		// Update the rotation and position of the sprite based on the delta
		// values. If the sprite
		// moves off the edge of the screen, it is wrapped around to the other
		// side.

		this.angle += this.deltaAngle;
		if (this.angle < 0)
			this.angle += 2 * Math.PI;
		if (this.angle > 2 * Math.PI)
			this.angle -= 2 * Math.PI;
		this.currentX += this.deltaX;
		if (this.currentX < -width / 2)
			this.currentX += width;
		if (this.currentX > width / 2)
			this.currentX -= width;
		this.currentY -= this.deltaY;
		if (this.currentY < -height / 2)
			this.currentY += height;
		if (this.currentY > height / 2)
			this.currentY -= height;
	}

	public void render() {
		// Render the sprite's shape and location by rotating it's base shape
		// and moving it to it's proper screen position.

		this.sprite = new Polygon();
		for (int i = 0; i < this.shape.npoints; i++)
			this.sprite.addPoint((int) Math.round(this.shape.xpoints[i]
					* Math.cos(this.angle) + this.shape.ypoints[i] // CHANGE
					// cos to
					// sin for a
					// fun
					// effect
					* Math.sin(this.angle))
					+ (int) Math.round(this.currentX) + width / 2, (int) Math
					.round(this.shape.ypoints[i] * Math.cos(this.angle)
							- this.shape.xpoints[i] * Math.sin(this.angle))
					+ (int) Math.round(this.currentY) + height / 2);
	}

	public boolean isColliding(AsteroidsSprite s) {

		// Determine if one sprite overlaps with another, i.e., if any vertice
		// of one sprite lands inside the other.

		for (int i = 0; i < s.sprite.npoints; i++)
			if (this.sprite.contains(s.sprite.xpoints[i], s.sprite.ypoints[i]))
				return true;
		for (int i = 0; i < this.sprite.npoints; i++)
			if (s.sprite.contains(this.sprite.xpoints[i],
					this.sprite.ypoints[i]))
				return true;
		return false;
	}
}
