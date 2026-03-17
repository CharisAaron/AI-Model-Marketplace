$files = @(
    "contracts\ai-marketplace-v2.clar",
    "contracts\sbtc-trait.clar",
    "contracts\ai-marketplace.clar"
)

foreach ($file in $files) {
    if (Test-Path $file) {
        $content = [System.IO.File]::ReadAllText($file)
        $utf8NoBom = New-Object System.Text.UTF8Encoding $false
        [System.IO.File]::WriteAllText($file, $content, $utf8NoBom)
        Write-Host "Fixed: $file"
    }
}
