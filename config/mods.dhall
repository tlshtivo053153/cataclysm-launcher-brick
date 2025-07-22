-- Example list of available mods
let ModType = < GitHub | TarGz >

in  [ { msiName = "Kenan's Modpack"
      , msiRepositoryName = "CDDA-Structured-Kenan-Modpack"
      , msiUrl = "https://github.com/Kenan2000/CDDA-Structured-Kenan-Modpack"
      , msiType = ModType.GitHub
      }
    , { msiName = "Another Example Mod"
      , msiRepositoryName = "AnotherMod"
      , msiUrl = "https://github.com/SomeUser/AnotherMod"
      , msiType = ModType.GitHub
      }
    , { msiName = "Another Example Mod2"
      , msiRepositoryName = "AnotherMod2"
      , msiUrl = "https://example.com/file.tar.gz"
      , msiType = ModType.TarGz
      }
    ]
