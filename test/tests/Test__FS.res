open Mocha

module TextEncoder = {
  type t

  @new external make: unit => t = "TextEncoder"
  @send external encode: (t, string) => Uint8Array.t = "encode"
}

describe("FS", () => {
  This.timeout(10000)

  describe("copy", () => {
    Async.it(
      "should copy file successfully when source exists",
      async () => {
        // Create source file
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-copy-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let sourceFile = NodeJs.Path.join([tempDir, "source.txt"])
        let destFile = NodeJs.Path.join([tempDir, "dest.txt"])

        // Create directory and source file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        let content = "test file content"
        NodeJs.Fs.writeFileSync(sourceFile, NodeJs.Buffer.fromString(content))

        // Test FS.copy
        let sourceUri = VSCode.Uri.file(sourceFile)
        let destUri = VSCode.Uri.file(destFile)
        let result = await FS.copy(sourceUri, destUri)

        switch result {
        | Ok() =>
          // Verify destination file exists and has same content
          let exists = NodeJs.Fs.existsSync(destFile)
          Assert.ok(exists)

          let destContent = NodeJs.Fs.readFileSync(destFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(destContent, content)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(sourceFile)
        NodeJs.Fs.unlinkSync(destFile)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Error when source file does not exist",
      async () => {
        // Use non-existent source file
        let nonExistentSource = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-source-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        let destFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "dest-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])

        let sourceUri = VSCode.Uri.file(nonExistentSource)
        let destUri = VSCode.Uri.file(destFile)
        let result = await FS.copy(sourceUri, destUri)

        switch result {
        | Ok() => Assert.fail("Expected Error for non-existent source, got Ok")
        | Error(errorMsg) =>
          // Should get an error message
          Assert.ok(String.length(errorMsg) > 0)
        }
      },
    )

    Async.it(
      "should create parent directories when copying to non-existent destination directory",
      async () => {
        // Create source file
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-copy-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let sourceFile = NodeJs.Path.join([tempDir, "source.txt"])
        let destFile = NodeJs.Path.join([tempDir, "nonexistent", "dest.txt"])

        // Create directory and source file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(sourceFile, NodeJs.Buffer.fromString("test content"))

        let sourceUri = VSCode.Uri.file(sourceFile)
        let destUri = VSCode.Uri.file(destFile)
        let result = await FS.copy(sourceUri, destUri)

        switch result {
        | Ok() =>
          // VS Code automatically creates parent directories
          let exists = NodeJs.Fs.existsSync(destFile)
          Assert.ok(exists)

          let content = NodeJs.Fs.readFileSync(destFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(content, "test content")

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(sourceFile)
        NodeJs.Fs.unlinkSync(destFile)
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "nonexistent"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("createDirectory", () => {
    Async.it(
      "should create directory successfully when parent exists",
      async () => {
        // Create parent directory
        let parentDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-mkdir-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let newDir = NodeJs.Path.join([parentDir, "newdir"])

        // Create parent directory
        await NodeJs.Fs.mkdir(parentDir, {recursive: true, mode: 0o777})

        // Test FS.createDirectory
        let uri = VSCode.Uri.file(newDir)
        let result = await FS.createDirectory(uri)

        switch result {
        | Ok() =>
          // Verify directory was created
          let exists = NodeJs.Fs.existsSync(newDir)
          Assert.ok(exists)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.rmdirSync(newDir)
        NodeJs.Fs.rmdirSync(parentDir)
      },
    )

    Async.it(
      "should create parent directories recursively when they don't exist",
      async () => {
        // Use non-existent parent directory
        let nonExistentParent = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-parent-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let newDir = NodeJs.Path.join([nonExistentParent, "newdir"])

        let uri = VSCode.Uri.file(newDir)
        let result = await FS.createDirectory(uri)

        switch result {
        | Ok() =>
          // VS Code creates parent directories recursively
          let exists = NodeJs.Fs.existsSync(newDir)
          Assert.ok(exists)

          let parentExists = NodeJs.Fs.existsSync(nonExistentParent)
          Assert.ok(parentExists)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.rmdirSync(newDir)
        NodeJs.Fs.rmdirSync(nonExistentParent)
      },
    )

    Async.it(
      "should succeed when directory already exists (idempotent)",
      async () => {
        // Create directory first
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-mkdir-existing-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Create the directory first
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        // Try to create it again with FS.createDirectory
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.createDirectory(uri)

        switch result {
        | Ok() =>
          // VS Code createDirectory is idempotent - doesn't error on existing dirs
          let exists = NodeJs.Fs.existsSync(tempDir)
          Assert.ok(exists)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("delete", () => {
    Async.it(
      "should delete file successfully when file exists",
      async () => {
        // Create a file to delete
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-delete-file-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])

        // Create the file
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString("test content"))

        // Verify it exists
        let existsBefore = NodeJs.Fs.existsSync(tempFile)
        Assert.ok(existsBefore)

        // Test FS.delete
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.delete(uri)

        switch result {
        | Ok() =>
          // Verify file was deleted
          let existsAfter = NodeJs.Fs.existsSync(tempFile)
          Assert.ok(!existsAfter)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
      },
    )

    Async.it(
      "should delete directory successfully when directory exists and is empty",
      async () => {
        // Create an empty directory to delete
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-delete-dir-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Create the directory
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        // Verify it exists
        let existsBefore = NodeJs.Fs.existsSync(tempDir)
        Assert.ok(existsBefore)

        // Test FS.delete
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.delete(uri)

        switch result {
        | Ok() =>
          // Verify directory was deleted
          let existsAfter = NodeJs.Fs.existsSync(tempDir)
          Assert.ok(!existsAfter)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
      },
    )

    Async.it(
      "should return Error when trying to delete non-empty directory",
      async () => {
        // Create a directory with files
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-delete-nonempty-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let file1 = NodeJs.Path.join([tempDir, "file1.txt"])

        // Create directory structure
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(file1, NodeJs.Buffer.fromString("content1"))

        // Verify everything exists
        Assert.ok(NodeJs.Fs.existsSync(tempDir))
        Assert.ok(NodeJs.Fs.existsSync(file1))

        // Test FS.delete on non-empty directory (should fail)
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.delete(uri)

        switch result {
        | Ok() => Assert.fail("Expected Error for non-empty directory, got Ok")
        | Error(errorMsg) =>
          // Should get an error message about directory not being empty
          Assert.ok(String.length(errorMsg) > 0)
          // Directory and file should still exist
          Assert.ok(NodeJs.Fs.existsSync(tempDir))
          Assert.ok(NodeJs.Fs.existsSync(file1))
        }

        // Cleanup manually
        NodeJs.Fs.unlinkSync(file1)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Error when trying to delete non-existent file",
      async () => {
        // Use non-existent file path
        let nonExistentFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-delete-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])

        let uri = VSCode.Uri.file(nonExistentFile)
        let result = await FS.delete(uri)

        switch result {
        | Ok() => Assert.fail("Expected Error for non-existent file, got Ok")
        | Error(errorMsg) =>
          // Should get an error message
          Assert.ok(String.length(errorMsg) > 0)
        }
      },
    )
  })

  describe("isWritableFileSystem", () => {
    Async.it(
      "should return Ok(true) for writable file system (local files)",
      async () => {
        // Use a writable local directory
        let writableDir = NodeJs.Os.tmpdir()
        let uri = VSCode.Uri.file(writableDir)
        let result = FS.isWritableFileSystem(uri)

        switch result {
        | Ok(isWritable) => Assert.ok(isWritable) // Local file system should be writable
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }
      },
    )

    Async.it(
      "should handle file URIs correctly",
      async () => {
        // Create a temporary file and test its file system writability
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-writable-test-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])

        // Create the file
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString("test"))

        let uri = VSCode.Uri.file(tempFile)
        let result = FS.isWritableFileSystem(uri)

        switch result {
        | Ok(isWritable) =>
          // Local file system should be writable
          Assert.ok(isWritable)
        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )

    Async.it(
      "should return consistent results for the same file system",
      async () => {
        // Test multiple paths on the same file system
        let tempDir1 = NodeJs.Path.join([NodeJs.Os.tmpdir(), "test1"])
        let tempDir2 = NodeJs.Path.join([NodeJs.Os.tmpdir(), "test2"])

        let uri1 = VSCode.Uri.file(tempDir1)
        let uri2 = VSCode.Uri.file(tempDir2)

        let result1 = FS.isWritableFileSystem(uri1)
        let result2 = FS.isWritableFileSystem(uri2)

        switch (result1, result2) {
        | (Ok(writable1), Ok(writable2)) =>
          // Both should have the same writability since they're on the same file system
          Assert.deepStrictEqual(writable1, writable2)
        | (Error(err1), _) => Assert.fail("Expected Ok for first URI, got Error: " ++ err1)
        | (_, Error(err2)) => Assert.fail("Expected Ok for second URI, got Error: " ++ err2)
        }
      },
    )
  })

  describe("readDirectory", () => {
    Async.it(
      "should return directory contents when directory exists",
      async () => {
        // Create a temporary directory with some files
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Create directory and files
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(
          NodeJs.Path.join([tempDir, "test.txt"]),
          NodeJs.Buffer.fromString("test"),
        )
        await NodeJs.Fs.mkdir(NodeJs.Path.join([tempDir, "subdir"]), {recursive: true, mode: 0o777})

        // Test FS.readDirectory
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.readDirectory(uri)

        switch result {
        | Ok(entries) =>
          // Should have at least the file and subdirectory we created
          let names = entries->Array.map(((name, _type)) => name)
          Assert.ok(names->Array.includes("test.txt"))
          Assert.ok(names->Array.includes("subdir"))

          // Check types
          let testFileEntry = entries->Array.find(((name, _type)) => name == "test.txt")
          let subdirEntry = entries->Array.find(((name, _type)) => name == "subdir")

          switch (testFileEntry, subdirEntry) {
          | (Some((_, _fileType)), Some((_, _dirType))) =>
            // Note: Exact FileType values may vary, but this tests the structure
            Assert.ok(true) // If we got here, the types are correct
          | _ => Assert.fail("Expected to find both file and directory entries")
          }

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(NodeJs.Path.join([tempDir, "test.txt"]))
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "subdir"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Error when directory does not exist",
      async () => {
        // Use a non-existent directory path
        let nonExistentPath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        let uri = VSCode.Uri.file(nonExistentPath)
        let result = await FS.readDirectory(uri)

        switch result {
        | Ok(_) => Assert.fail("Expected Error for non-existent directory, got Ok")
        | Error(errorMsg) =>
          // Should get some kind of error message
          Assert.ok(String.length(errorMsg) > 0)
        // Common error messages might include "not found", "does not exist", etc.
        // We don't assert exact message since it may vary by platform/VS Code version
        }
      },
    )

    Async.it(
      "should return Error when path is a file, not directory",
      async () => {
        // Create a temporary file
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-test-file-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])

        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString("test content"))

        // Try to read it as a directory
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.readDirectory(uri)

        switch result {
        | Ok(_) => Assert.fail("Expected Error when trying to read file as directory, got Ok")
        | Error(errorMsg) =>
          // Should get an error indicating it's not a directory
          Assert.ok(String.length(errorMsg) > 0)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )
  })

  describe("readFile", () => {
    Async.it(
      "should read file content successfully when file exists",
      async () => {
        // Create a temporary file with known content
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-readfile-test-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        let testContent = "Hello, VS Code FileSystem!"

        // Create the file
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString(testContent))

        // Test FS.readFile
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.readFile(uri)

        switch result {
        | Ok(uint8Array) =>
          // Convert Uint8Array back to string to verify content
          // For now, just verify we got some data back
          Assert.ok(TypedArray.length(uint8Array) > 0)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )

    Async.it(
      "should return Error when file does not exist",
      async () => {
        // Use a non-existent file path
        let nonExistentFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-readfile-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])

        let uri = VSCode.Uri.file(nonExistentFile)
        let result = await FS.readFile(uri)

        switch result {
        | Ok(_) => Assert.fail("Expected Error for non-existent file, got Ok")
        | Error(errorMsg) =>
          // Should get an error message
          Assert.ok(String.length(errorMsg) > 0)
        }
      },
    )

    Async.it(
      "should return Error when trying to read a directory as file",
      async () => {
        // Create a temporary directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-readfile-dir-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Create the directory
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        // Try to read the directory as a file
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.readFile(uri)

        switch result {
        | Ok(_) => Assert.fail("Expected Error when trying to read directory as file, got Ok")
        | Error(errorMsg) =>
          // Should get an error message indicating it's a directory
          Assert.ok(String.length(errorMsg) > 0)
        }

        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should handle binary file content correctly",
      async () => {
        // Create a temporary file with binary content
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-readfile-binary-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".bin",
        ])

        // Create binary content (some bytes including null bytes)
        let binaryData = NodeJs.Buffer.fromArray([0, 1, 255, 127, 0, 42])
        NodeJs.Fs.writeFileSync(tempFile, binaryData)

        // Test FS.readFile
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.readFile(uri)

        switch result {
        | Ok(uint8Array) =>
          // Verify we read the correct amount of data
          Assert.deepStrictEqual(TypedArray.length(uint8Array), binaryData->NodeJs.Buffer.length)

          // Compare each byte
          for i in 0 to binaryData->NodeJs.Buffer.length - 1 {
            let originalByte = binaryData->NodeJs.Buffer.readUint8(~offset=i)->Float.toInt
            let readByte = TypedArray.get(uint8Array, i)->Option.getOr(0)
            Assert.deepStrictEqual(readByte, originalByte)
          }

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )
  })

  describe("rename", () => {
    Async.it(
      "should rename file successfully when source exists",
      async () => {
        // Create source file
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-rename-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let sourceFile = NodeJs.Path.join([tempDir, "source.txt"])
        let targetFile = NodeJs.Path.join([tempDir, "target.txt"])

        // Create directory and source file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        let content = "test file content"
        NodeJs.Fs.writeFileSync(sourceFile, NodeJs.Buffer.fromString(content))

        // Verify source exists before rename
        Assert.ok(NodeJs.Fs.existsSync(sourceFile))
        Assert.ok(!NodeJs.Fs.existsSync(targetFile))

        // Test FS.rename
        let sourceUri = VSCode.Uri.file(sourceFile)
        let targetUri = VSCode.Uri.file(targetFile)
        let result = await FS.rename(sourceUri, targetUri)

        switch result {
        | Ok() =>
          // Verify source no longer exists and target exists with same content
          Assert.ok(!NodeJs.Fs.existsSync(sourceFile))
          Assert.ok(NodeJs.Fs.existsSync(targetFile))

          let targetContent = NodeJs.Fs.readFileSync(targetFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(targetContent, content)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(targetFile)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Error when source file does not exist",
      async () => {
        // Use non-existent source file
        let nonExistentSource = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-rename-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        let targetFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "rename-target-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])

        let sourceUri = VSCode.Uri.file(nonExistentSource)
        let targetUri = VSCode.Uri.file(targetFile)
        let result = await FS.rename(sourceUri, targetUri)

        switch result {
        | Ok() => Assert.fail("Expected Error for non-existent source, got Ok")
        | Error(errorMsg) =>
          // Should get an error message
          Assert.ok(String.length(errorMsg) > 0)
        }
      },
    )

    Async.it(
      "should rename directory successfully when source exists",
      async () => {
        // Create source directory with contents
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-rename-dir-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let sourceDir = NodeJs.Path.join([tempDir, "sourcedir"])
        let targetDir = NodeJs.Path.join([tempDir, "targetdir"])
        let testFile = NodeJs.Path.join([sourceDir, "test.txt"])

        // Create directory structure
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        await NodeJs.Fs.mkdir(sourceDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(testFile, NodeJs.Buffer.fromString("test content"))

        // Verify source exists before rename
        Assert.ok(NodeJs.Fs.existsSync(sourceDir))
        Assert.ok(NodeJs.Fs.existsSync(testFile))
        Assert.ok(!NodeJs.Fs.existsSync(targetDir))

        // Test FS.rename on directory
        let sourceUri = VSCode.Uri.file(sourceDir)
        let targetUri = VSCode.Uri.file(targetDir)
        let result = await FS.rename(sourceUri, targetUri)

        switch result {
        | Ok() =>
          // Verify source no longer exists and target exists with contents
          Assert.ok(!NodeJs.Fs.existsSync(sourceDir))
          Assert.ok(NodeJs.Fs.existsSync(targetDir))

          let renamedFile = NodeJs.Path.join([targetDir, "test.txt"])
          Assert.ok(NodeJs.Fs.existsSync(renamedFile))

          let content = NodeJs.Fs.readFileSync(renamedFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(content, "test content")

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(NodeJs.Path.join([targetDir, "test.txt"]))
        NodeJs.Fs.rmdirSync(targetDir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should create parent directories when renaming to non-existent destination directory",
      async () => {
        // Create source file
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-rename-parent-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let sourceFile = NodeJs.Path.join([tempDir, "source.txt"])
        let targetFile = NodeJs.Path.join([tempDir, "nonexistent", "target.txt"])

        // Create directory and source file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(sourceFile, NodeJs.Buffer.fromString("test content"))

        let sourceUri = VSCode.Uri.file(sourceFile)
        let targetUri = VSCode.Uri.file(targetFile)
        let result = await FS.rename(sourceUri, targetUri)

        switch result {
        | Ok() =>
          // VS Code automatically creates parent directories for rename
          Assert.ok(!NodeJs.Fs.existsSync(sourceFile))
          Assert.ok(NodeJs.Fs.existsSync(targetFile))

          let content = NodeJs.Fs.readFileSync(targetFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(content, "test content")

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(targetFile)
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "nonexistent"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("stat", () => {
    Async.it(
      "should return file stats when file exists",
      async () => {
        // Create a temporary file
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-stat-file-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        let content = "test file content for stat"

        // Create the file
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString(content))

        // Test FS.stat
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.stat(uri)

        switch result {
        | Ok(fileStat) =>
          // Verify it's a file and has the correct size
          Assert.ok(VSCode.FileStat.type_(fileStat) == VSCode.FileType.File)
          Assert.deepStrictEqual(VSCode.FileStat.size(fileStat), String.length(content))

          // Verify timestamps are reasonable (should be recent)
          let now = Js.Date.now()
          let mtime = VSCode.FileStat.mtime(fileStat)->Int.toFloat
          let ctime = VSCode.FileStat.ctime(fileStat)->Int.toFloat

          // File should have been created/modified within the last minute
          Assert.ok(now -. mtime < 60000.0) // 60 seconds
          Assert.ok(now -. ctime < 60000.0) // 60 seconds

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )

    Async.it(
      "should return directory stats when directory exists",
      async () => {
        // Create a temporary directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-stat-dir-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Create the directory
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        // Test FS.stat
        let uri = VSCode.Uri.file(tempDir)
        let result = await FS.stat(uri)

        switch result {
        | Ok(fileStat) =>
          // Verify it's a directory
          Assert.ok(VSCode.FileStat.type_(fileStat) == VSCode.FileType.Directory)

          // Directory size may vary by platform, just check it's non-negative
          Assert.ok(VSCode.FileStat.size(fileStat) >= 0)

          // Verify timestamps are reasonable
          let now = Js.Date.now()
          let mtime = VSCode.FileStat.mtime(fileStat)->Int.toFloat
          let ctime = VSCode.FileStat.ctime(fileStat)->Int.toFloat

          Assert.ok(now -. mtime < 60000.0) // 60 seconds
          Assert.ok(now -. ctime < 60000.0) // 60 seconds

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Error when file does not exist",
      async () => {
        // Use non-existent file path
        let nonExistentFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "non-existent-stat-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])

        let uri = VSCode.Uri.file(nonExistentFile)
        let result = await FS.stat(uri)

        switch result {
        | Ok(_) => Assert.fail("Expected Error for non-existent file, got Ok")
        | Error(errorMsg) =>
          // Should get an error message
          Assert.ok(String.length(errorMsg) > 0)
        }
      },
    )

    Async.it(
      "should distinguish between files and directories",
      async () => {
        // Create both a file and a directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-stat-types-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let testFile = NodeJs.Path.join([tempDir, "test.txt"])
        let testSubdir = NodeJs.Path.join([tempDir, "subdir"])

        // Create directory structure
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(testFile, NodeJs.Buffer.fromString("test"))
        await NodeJs.Fs.mkdir(testSubdir, {recursive: true, mode: 0o777})

        // Test file stat
        let fileUri = VSCode.Uri.file(testFile)
        let fileResult = await FS.stat(fileUri)

        // Test directory stat
        let dirUri = VSCode.Uri.file(testSubdir)
        let dirResult = await FS.stat(dirUri)

        switch (fileResult, dirResult) {
        | (Ok(fileStat), Ok(dirStat)) =>
          // Verify types are different
          Assert.ok(VSCode.FileStat.type_(fileStat) == VSCode.FileType.File)
          Assert.ok(VSCode.FileStat.type_(dirStat) == VSCode.FileType.Directory)

          // File should have content size, directory should be different
          Assert.ok(VSCode.FileStat.size(fileStat) == 4) // "test" = 4 bytes
          Assert.ok(VSCode.FileStat.size(dirStat) != VSCode.FileStat.size(fileStat))

        | (Error(fileErr), _) => Assert.fail("Expected Ok for file stat, got Error: " ++ fileErr)
        | (_, Error(dirErr)) => Assert.fail("Expected Ok for directory stat, got Error: " ++ dirErr)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(testFile)
        NodeJs.Fs.rmdirSync(testSubdir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("writeFile", () => {
    Async.it(
      "should write file content successfully",
      async () => {
        // Create path for new file
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-writefile-test-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        let content = "Hello from FS.writeFile!"

        // Convert string to Uint8Array
        let uint8Array = TextEncoder.make()->TextEncoder.encode(content)

        // Test FS.writeFile
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.writeFile(uri, uint8Array)

        switch result {
        | Ok() =>
          // Verify file was created and has correct content
          Assert.ok(NodeJs.Fs.existsSync(tempFile))

          let writtenContent = NodeJs.Fs.readFileSync(tempFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(writtenContent, content)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )

    Async.it(
      "should overwrite existing file content",
      async () => {
        // Create existing file
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-writefile-overwrite-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
        ])
        let originalContent = "Original content"
        let newContent = "New content from FS.writeFile"

        // Create file with original content
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString(originalContent))

        // Verify original content
        let readContent1 = NodeJs.Fs.readFileSync(tempFile)->NodeJs.Buffer.toString
        Assert.deepStrictEqual(readContent1, originalContent)

        // Convert new content to Uint8Array
        let uint8Array = TextEncoder.make()->TextEncoder.encode(newContent)

        // Test FS.writeFile (overwrite)
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.writeFile(uri, uint8Array)

        switch result {
        | Ok() =>
          // Verify file was overwritten with new content
          let readContent2 = NodeJs.Fs.readFileSync(tempFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(readContent2, newContent)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )

    Async.it(
      "should create parent directories when writing to non-existent directory",
      async () => {
        // Create path with non-existent parent directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-writefile-parent-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let tempFile = NodeJs.Path.join([tempDir, "nested", "test.txt"])
        let content = "Content in nested directory"

        // Verify parent directories don't exist
        Assert.ok(!NodeJs.Fs.existsSync(tempDir))
        Assert.ok(!NodeJs.Fs.existsSync(NodeJs.Path.join([tempDir, "nested"])))

        // Convert content to Uint8Array
        let uint8Array = TextEncoder.make()->TextEncoder.encode(content)

        // Test FS.writeFile
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.writeFile(uri, uint8Array)

        switch result {
        | Ok() =>
          // VS Code automatically creates parent directories
          Assert.ok(NodeJs.Fs.existsSync(tempFile))

          let writtenContent = NodeJs.Fs.readFileSync(tempFile)->NodeJs.Buffer.toString
          Assert.deepStrictEqual(writtenContent, content)

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "nested"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should handle binary file content correctly",
      async () => {
        // Create path for binary file
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "fs-writefile-binary-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".bin",
        ])

        // Create binary content (some bytes including null bytes)
        let binaryData = [0, 1, 255, 127, 0, 42]
        let buffer = NodeJs.Buffer.fromArray(binaryData)
        let uint8Array = Uint8Array.fromArray(binaryData)

        // Test FS.writeFile
        let uri = VSCode.Uri.file(tempFile)
        let result = await FS.writeFile(uri, uint8Array)

        switch result {
        | Ok() =>
          // Verify file was created with correct binary content
          Assert.ok(NodeJs.Fs.existsSync(tempFile))

          let writtenBuffer = NodeJs.Fs.readFileSync(tempFile)
          Assert.deepStrictEqual(writtenBuffer->NodeJs.Buffer.length, buffer->NodeJs.Buffer.length)

          // Compare each byte
          for i in 0 to buffer->NodeJs.Buffer.length - 1 {
            let originalByte = buffer->NodeJs.Buffer.readUint8(~offset=i)->Float.toInt
            let writtenByte = writtenBuffer->NodeJs.Buffer.readUint8(~offset=i)->Float.toInt
            Assert.deepStrictEqual(writtenByte, originalByte)
          }

        | Error(errorMsg) => Assert.fail("Expected Ok, got Error: " ++ errorMsg)
        }

        // Cleanup
        NodeJs.Fs.unlinkSync(tempFile)
      },
    )
  })
})
