// Manual test to demonstrate the Unicode position issue #250
// This can be run independently to see the UTF-16 vs logical position mismatch

// Test string with mathematical bold characters (non-BMP Unicode)
const testCases = [
  {
    name: "Single mathematical bold char",
    text: "𝐱 = {!   !}",
    expectedGoalStart: 4,  // Logical position after "𝐱 = "
    description: "Mathematical bold 𝐱 takes 2 UTF-16 code units but 1 logical char"
  },
  {
    name: "Multiple mathematical bold chars", 
    text: "𝐱𝐲𝐳 = {!   !}",
    expectedGoalStart: 6,  // Logical position after "𝐱𝐲𝐳 = "
    description: "Three mathematical bold chars, each taking 2 UTF-16 code units"
  },
  {
    name: "Mixed Unicode and ASCII",
    text: "𝐍ormal-text-then-𝐱 = {!   !}",
    expectedGoalStart: 20, // Logical position after the mixed text
    description: "Mix of mathematical bold chars and regular ASCII"
  },
  {
    name: "Regular ASCII for comparison",
    text: "x = {!   !}",
    expectedGoalStart: 4,  // Logical position after "x = "
    description: "Control case with no Unicode issues"
  }
];

console.log("Unicode Position Testing for Issue #250");
console.log("======================================");

testCases.forEach((testCase, index) => {
  console.log(`\nTest ${index + 1}: ${testCase.name}`);
  console.log(`Text: "${testCase.text}"`);
  console.log(`Description: ${testCase.description}`);
  
  // Simulate how positions are calculated in different ways
  const utf16Length = testCase.text.indexOf("{!   !}");
  const logicalLength = [...testCase.text].indexOf("{!   !}");
  const expectedStart = testCase.expectedGoalStart;
  
  console.log(`UTF-16 position (VSCode): ${utf16Length}`);
  console.log(`Logical position (expected): ${logicalLength}`);
  console.log(`Expected goal start: ${expectedStart}`);
  console.log(`Position difference: ${utf16Length - logicalLength}`);
  
  if (utf16Length !== logicalLength) {
    console.log(`❌ MISMATCH detected! This would cause hole misplacement.`);
  } else {
    console.log(`✅ Positions match - no issue for this case.`);
  }
});

console.log("\n" + "=".repeat(50));
console.log("Summary:");
console.log("- Mathematical bold chars (𝐱, 𝐲, 𝐳, etc.) are non-BMP Unicode");
console.log("- They use UTF-16 surrogate pairs (2 code units per character)");
console.log("- VSCode uses UTF-16 internally, Agda likely uses logical positions");
console.log("- This causes position drift when Unicode chars precede goals");
console.log("- The drift equals the number of non-BMP Unicode characters");

// Show the actual UTF-16 breakdown
console.log("\nUTF-16 Code Unit Breakdown:");
testCases.forEach((testCase, index) => {
  if (testCase.text.includes('𝐱') || testCase.text.includes('𝐲') || testCase.text.includes('𝐳') || testCase.text.includes('𝐍')) {
    console.log(`\nTest ${index + 1} - "${testCase.text}":`);
    const chars = [...testCase.text];
    const utf16Units = [];
    for (let i = 0; i < testCase.text.length; i++) {
      utf16Units.push(testCase.text.charCodeAt(i));
    }
    console.log(`Logical chars (${chars.length}):`, chars);
    console.log(`UTF-16 units (${utf16Units.length}):`, utf16Units);
  }
});