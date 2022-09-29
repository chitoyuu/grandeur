extends VBoxContainer

onready var mp = get_node("mp")
onready var player = get_node("player")

func _ready():
	player.play("in")

func _grandeur_unmount():
	player.play("out")
	yield(player, "animation_finished")
	self.get_parent().remove_child(self)
	self.queue_free()

func _grandeur_mount_point():
	return mp