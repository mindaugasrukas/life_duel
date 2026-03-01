# Bouncing Ball

## Design Goal

This architecture is designed to produce a system that is clear, extendable, readable, and maintainable.

## High-Level Architecture

### AppController

Composes all modules and wires dependencies.
Owns startup, mode transitions, and main loop orchestration.

### GameEngine

Owns authoritative runtime simulation state.
Advances state each tick (step(dt)), applies rules, collisions, win/loss.
Exposes read-only snapshot/selectors for rendering.
No DOM/canvas drawing.

### GameRenderer

Renders game world/canvas from engine snapshot.
Handles visual layers: background, features, entities, particles, trajectory, overlays.
Stateless or minimally stateful (animation phase/cache only).
No gameplay mutation.

Provides a unified render API used by all modes:
- `renderMain(scene, viewConfig)`
- `renderPreview(scene, previewConfig, targetCanvas)`
- `renderOverlay(scene, overlayConfig)`

### UIController

Renders non-canvas UI (HUD, buttons, messages, mode panels).
Registers and routes input events to mode/app actions.
No direct game-rule decisions.

### LevelService

Level loading, normalization, procedural generation, feature instantiation.
Returns pure level data/config to engine.

### Feature System

FeatureRegistry + feature interfaces:
* BallFeature (ball interactions),
* ObstacleFeature (collision/geometry behavior),
* LauncherFeature (launch modifiers).
Feature logic affects simulation through engine-defined hooks.

FeatureRegistry ownership and usage:
- `FeatureRegistry` is provided as a dependency to `GameEngine`.
- `GameEngine` is the only module that instantiates and executes feature hooks.
- `LevelService` provides pure feature config; it does not execute feature behavior.

### Mode Controllers

PlayModeController, InspectorModeController, CreatorModeController.
Mode-specific UI workflows and commands.
Can request renderer overlays and engine actions through clear APIs.

Mode controllers provide render intent/configuration, not drawing logic:
- `PlayModeController` supplies `viewConfig = play`
- `InspectorModeController` supplies `previewConfig = inspectorCard`
- `CreatorModeController` supplies `overlayConfig = creatorGuides`

## Shared Types / Contracts

- `GameState` — authoritative simulation state owned by `GameEngine`.
- `RenderState` — renderer-oriented snapshot derived from `GameState`.
- `UIState` — HUD/panel state rendered by `UIController`.
- `InputAction` — normalized user intent dispatched by `AppController`.
- `LevelConfig` — normalized level data from `LevelService`.
- `FeatureContext` — constrained API surface exposed to feature hooks.
- `ModeContext` — mode lifecycle context (`enter/exit/update/handleAction`).

## Interfaces (Minimal Contracts)

### AppController

- `bootstrap()`
- `dispatch(action: InputAction)`
- `switchMode(nextMode, context)`
- `startLoop()`

### GameEngine

- `init(level: LevelConfig)`
- `step(dt)`
- `applyAction(action: InputAction)`
- `getSnapshot(): RenderState`
- `on(eventName, handler)`

### GameRenderer

- `renderMain(scene, viewConfig)`
- `renderPreview(scene, previewConfig, targetCanvas)`
- `renderOverlay(scene, overlayConfig)`

### UIController

- `bindInputs(dispatch)`
- `renderHUD(uiState)`
- `renderPanels(uiState)`
- `showMessage(text, duration)`

### LevelService

- `getLevel(levelId): LevelConfig`
- `getLevelSnapshot(levelId)`
- `generateLevel(seedOrIndex): LevelConfig`
- `normalizeLevel(rawLevel): LevelConfig`

### ModeController

- `enter(context: ModeContext)`
- `handleAction(action: InputAction)`
- `update(dt)` (optional)
- `exit()`

## Architecture Constraints

### Controller-Only Flow (No Global Functionality)

- No gameplay or mode logic may be exposed as global functions.
- No gameplay state may be mirrored to `window` (no alias-based global state access).
- No inline HTML event handlers (e.g., `onclick` attributes).
- All user input must flow through:
	`UIController -> AppController -> ActiveModeController -> GameEngine/LevelService`.
- All world rendering must flow through:
	`AppController -> GameRenderer`.
- Mode controllers provide intent/config only; they do not directly mutate DOM or engine internals.
- URL/query synchronization must be handled by a dedicated controller/service (not ad-hoc `window.location` writes).

### Allowed Global Surface

- At most one bootstrap entry point (or module self-init).
- Analytics/vendor snippets may remain isolated and must not access game state.

### State Ownership Rules

- `GameEngine` owns simulation state.
- `AppController` owns app/mode state.
- `UIController` owns UI view state only.
- Shared state is accessed via explicit APIs/selectors, never via globals.

### Dependency Direction Rules

- `UIController` does not mutate engine internals directly; it only dispatches `InputAction`.
- `GameRenderer` is read-only against simulation; it never mutates gameplay state.
- `GameEngine` has no DOM/UI dependency.
- Mode controllers orchestrate mode behavior but do not implement world drawing.

## Renderer Reuse Strategy (Main + Preview + Overlay)

### Core Principle

- Mode controllers decide **what** should be shown.
- `GameRenderer` decides **how** it is drawn.
- `GameEngine` decides **what state exists**.

### Shared Rendering Pipeline

- Preview rendering uses the same world-drawing pipeline as main rendering.
- Differences between main and preview are handled through config:
	- camera/scale
	- enabled visual layers
	- optional effect quality level
	- animation phase behavior

### Required Service Contracts

- `LevelService.getLevelSnapshot(levelId)` returns pure level snapshot data.
- `GameRenderer.renderPreview(snapshot, opts)` renders a preview without mutating game state.
- `UIController` owns preview canvas lifecycle in the DOM and invokes renderer methods.

### Anti-Duplication Rules

- No mode controller draws game geometry directly.
- No mode-specific copy of wall/target/feature drawing logic is allowed.
- World rendering code exists in one place: `GameRenderer`.
- Overlay code that depends on mode is routed through `GameRenderer.renderOverlay(...)`.

## Component Interaction Model

### Startup Flow

1. `AppController` boots the app.
2. It creates and wires: `GameEngine`, `GameRenderer`, `UIController`, `LevelService`, `FeatureRegistry`, and mode controllers.
3. `LevelService` provides the initial `LevelConfig`.
4. `GameEngine` is initialized with level data and enters initial mode (`play`, `inspector`, or `creator`).
5. Main loop begins:
	 - `GameEngine.step(dt)` updates simulation.
	 - `GameRenderer.renderMain(scene, viewConfig)` draws canvas.
	 - `UIController.renderHUD(uiState)` and `UIController.renderPanels(uiState)` update UI.

### Runtime Responsibilities and Data Flow

- `UIController` emits user intent as `InputAction` (shoot, reset, next level, switch mode, creator tool actions).
- `AppController` routes actions to the active mode controller.
- Mode controller validates context and calls `GameEngine` / `LevelService` operations.
- `GameEngine` mutates only simulation state and emits events (`onWin`, `onLose`, `onTargetHit`, `onLevelLoaded`).
- `AppController` handles events and triggers:
	- UI updates through `UIController`
	- visual updates through `GameRenderer` (main, preview, and overlays)
	- mode/level transitions through mode controllers + `LevelService`

### Action/Event Contract (Core Flows)

| Trigger | Action/Event | Producer | Consumer | Side Effect |
|---|---|---|---|---|
| Player presses shoot | `InputAction.SHOOT` | UIController | AppController -> PlayModeController -> GameEngine | Ball launch simulation starts |
| Player resets level | `InputAction.RESET_LEVEL` | UIController | AppController -> ActiveModeController -> LevelService/GameEngine | Current level state reloaded |
| Player switches mode | `InputAction.SWITCH_MODE` | UIController | AppController | Mode lifecycle transition (`exit`/`enter`) |
| Targets completed | `Event.WIN` | GameEngine | AppController | UI success message + next-level affordance |
| Danger/fail condition | `Event.LOSE` | GameEngine | AppController | Retry affordance + feedback |
| Level loaded | `Event.LEVEL_LOADED` | GameEngine/LevelService | AppController -> Renderer/UI | Scene + HUD refresh |
| Inspector requests preview | `InputAction.REQUEST_PREVIEW` | UIController/InspectorModeController | AppController -> LevelService -> GameRenderer | Preview card canvas rendered |

### Feature Interaction Contract

- `FeatureRegistry` maps `featureType` -> feature implementation.
- During level load, `GameEngine` instantiates feature objects via registry.
- On each step, engine executes ordered hooks:
	- pre-step hooks (optional)
	- collision/interaction hooks (`BallFeature`, `ObstacleFeature`, `LauncherFeature`)
	- post-step hooks (optional)
- Features influence simulation only through `FeatureContext` APIs (no direct DOM access).

## Game Mode Management

### Mode Ownership

- `AppController` owns current mode.
- Mode controllers own mode-specific behavior:
	- `PlayModeController`
	- `InspectorModeController`
	- `CreatorModeController`

### Mode Lifecycle

Each mode supports:

- `enter(context)`
- `handleAction(action)`
- `update(dt)` (optional)
- `exit()`

### Mode Transition Rules

- Transitions happen only through `AppController.switchMode(nextMode, context)`.
- `switchMode` performs:
	1. current mode `exit()`
	2. shared cleanup/sync
	3. next mode `enter(context)`
	4. UI + renderer refresh

### Typical Mode Behaviors

- **Play Mode**
	- Uses full engine simulation and HUD.
	- Supports shoot/reset/next level.
- **Inspector Mode**
	- Freezes gameplay simulation.
	- Renders level previews through `GameRenderer.renderPreview(...)` and shows metadata.
	- Allows quick jump to selected level.
- **Creator Mode**
	- Uses editor tools and placement interactions.
	- Uses renderer overlays for placement guides and tool feedback.
	- Can toggle test play without leaving creator context.
	- Supports export/import level config.

## User Experience (UX) Vision

### Overall UX Principles

- Immediate responsiveness (input feedback within same frame).
- Clear state visibility (mode, level, shots, score, status).
- Predictable controls (same action mapping across modes where possible).
- Safe transitions (no hidden state loss when switching modes).

### Player Journey

1. Open game and immediately enter play.
2. Aim and shoot with visible trajectory feedback.
3. Receive clear outcomes:
	 - success (`Clear`, next-level action)
	 - failure (`Danger`, retry action)
4. Optional branches:
	 - inspect levels and jump to one
	 - create/edit level and test instantly

### Mode-Specific UX

- **Play UX**
	- Minimal HUD clutter.
	- High-contrast active feedback (hit, bounce, portal, fail).
- **Inspector UX**
	- Dense information, quick browse, low interaction cost.
- **Creator UX**
	- Tool-first workflow:
		- select tool
		- place/drag geometry
		- instant visual preview
		- test and iterate

### Feedback System

- Visual: particles, pulses, overlays, trajectory.
- UI: concise messages and stat updates.
- Structural: clear mode indicator + explicit transition actions.
