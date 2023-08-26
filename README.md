# MX2FN

A small utility program for conversion of math expressions from Maxima to functions in various programming languages.

Sometimes I find myself prototyping math expressions in wxMaxima just to use them later in code. It can then be quite hassle to convert them, so this should make it a bit easier.

I also wanted to try out Haskell, so don't take the code here very seriously.

## Usage

### Javascript

```bash
echo "2*(a+b)^2" | mx2fn --js myFuncName
```

**Will output:**

```javascript
function myFuncName (a, b) {
  return 2.0 * (a + b) ** 2.0;
}
```

### Typescript

```bash
echo "2*(a+b)^2" | mx2fn --ts myFuncName
```

**Will output:**

```typescript
function myFuncName (a: number, b: number): number {
  return 2.0 * (a + b) ** 2.0;
}
```

### Rust

```bash
echo "2*(a+b)^2" | mx2fn --rs my_func_name
```

**Will output:**

```rust
fn my_func_name (a: f32, b: f32) -> f32 {
  2.0 * (a + b).powf(2.0)
}
```

