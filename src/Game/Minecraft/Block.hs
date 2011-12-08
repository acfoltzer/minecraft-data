-- | Defined based on
-- <http://www.minecraftwiki.net/wiki/Data_values#Block_IDs> as of 07
-- Dec 2011

module Game.Minecraft.Block where

-- | Relies on the derived Enum ordering to get the right values
data BlockId = Air | Stone | Grass | Dirt | Cobblestone | WoodenPlank | Sapling
             | Bedrock | WaterNoSpread | Water | LavaNoSpread | Lava | Sand 
             | Gravel | GoldOre | IronOre | CoalOre | Wood | Leaves | Sponge 
             | Glass | LapisLazuliOre | LapisLazuliBlock | Dispenser | Sandstone
             | NoteBlock | BedBlock | PoweredRail | DetectorRail | StickyPiston 
             | Web | TallGrass | DeadShrub | Piston | PistonHead | Wool | Unused
             | Dandelion | Rose | BrownMushroom | RedMushroom | BlockofGold 
             | BlockofIron | StoneSlabDouble | StoneSlab | Brick | TNT 
             | Bookcase | MossStone | Obsidian | Torch | Fire | MobSpawner 
             | WoodenStairs | Chest | RedstoneWire | DiamondOre | BlockofDiamond 
             | Workbench | WheatCrop | Farmland | Furnace | FurnaceSmelting 
             | SignBlock | WoodDoorBlock | Ladder | Rails | CobblestoneStairs 
             | SignWallBlock | Lever | StonePressurePlate | IronDoorBlock 
             | WoodenPressurePlate | RedstoneOre | RedstoneOreGlowing 
             | RedstoneTorchOff | RedstoneTorch | StoneButton | Snow | Ice 
             | SnowBlock | Cactus | ClayBlock | SugarCaneBlock | Jukebox 
             | Fence | Pumpkin | Netherrack | SoulSand | Glowstone | Portal 
             | JackOLantern | CakeBlock | RedstoneRepeaterBlockOff 
             | RedstoneRepeaterBlockOn | LockedChest | Trapdoor | SilverfishStone 
             | StoneBricks | BrownMushroomBlock | RedMushroomBlock | IronBars 
             | GlassPane  | MelonBlock | PumpkinVine | MelonVine | Vines 
             | FenceGate | BrickStairs | StoneBrickStairs | Mycelium | LilyPad 
             | NetherBrick | NetherBrickFence | NetherBrickStairs | NetherWart 
             | EnchantmentTable | BrewingStandBlock | CauldronBlock | EndPortal 
             | EndPortalFrame | EndStone | DragonEgg
               deriving (Show, Ord, Eq, Enum)