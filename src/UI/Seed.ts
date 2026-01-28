import { renderAppFrame } from "./Frame"

export function renderSeedView(): HTMLElement {
    const content = `
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Seed Phrase Entry</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            padding: 20px;
            background-color: #f4f4f9;
        }

        .seed-container {
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 15px;
            max-width: 700px;
            margin: 0 auto;
            padding: 20px;
            background-color: #fff;
            border-radius: 10px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        }

        .input-group {
            display: flex;
            align-items: center;
        }

        .input-group label {
            margin-right: 10px;
            font-weight: bold;
            color: #555;
            min-width: 20px;
            text-align: right;
        }

        .seed-input {
            width: 100%;
            padding: 8px;
            border: 1px solid #ccc;
            border-radius: 15px;
            box-sizing: border-box;
            font-size: 16px;
            transition: border-color 0.3s;
        }

        .seed-input:focus {
            border-color: #007bff;
            outline: none;
        }

        .submit-button-container {
            text-align: center;
            margin-top: 30px;
            max-width: 700px;
            margin-left: auto;
            margin-right: auto;
        }

        .submit-button {
            padding: 10px 25px;
            background-color: #007bff;
            color: white;
            border: none;
            border-radius: 20px;
            cursor: pointer;
            font-size: 18px;
            transition: background-color 0.3s, transform 0.1s;
        }

        .submit-button:hover {
            background-color: #0056b3;
        }

        .submit-button:active {
            transform: scale(0.98);
        }
    </style>
</head>
<body>

    <div class="seed-container">
        <div class="input-group">
            <label for="word1">1.</label>
            <input type="text" id="word1" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word2">2.</label>
            <input type="text" id="word2" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word3">3.</label>
            <input type="text" id="word3" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word4">4.</label>
            <input type="text" id="word4" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word5">5.</label>
            <input type="text" id="word5" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word6">6.</label>
            <input type="text" id="word6" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word7">7.</label>
            <input type="text" id="word7" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word8">8.</label>
            <input type="text" id="word8" class="seed-input" required>
        </div>

        <div class="input-group">
            <label for="word9">9.</label>
            <input type="text" id="word9" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word10">10.</label>
            <input type="text" id="word10" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word11">11.</label>
            <input type="text" id="word11" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word12">12.</label>
            <input type="text" id="word12" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word13">13.</label>
            <input type="text" id="word13" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word14">14.</label>
            <input type="text" id="word14" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word15">15.</label>
            <input type="text" id="word15" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word16">16.</label>
            <input type="text" id="word16" class="seed-input" required>
        </div>

        <div class="input-group">
            <label for="word17">17.</label>
            <input type="text" id="word17" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word18">18.</label>
            <input type="text" id="word18" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word19">19.</label>
            <input type="text" id="word19" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word20">20.</label>
            <input type="text" id="word20" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word21">21.</label>
            <input type="text" id="word21" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word22">22.</label>
            <input type="text" id="word22" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word23">23.</label>
            <input type="text" id="word23" class="seed-input" required>
        </div>
        <div class="input-group">
            <label for="word24">24.</label>
            <input type="text" id="word24" class="seed-input" required>
        </div>
    </div>

    <div class="submit-button-container">
        <button type="submit" class="submit-button">Submit Seed Phrase</button>
    </div>

</body>
</html>
`
    return renderAppFrame(content, false)
}
