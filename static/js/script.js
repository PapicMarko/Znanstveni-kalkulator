(function() {
    window.insertTextAtCursor = function(inputId, insertedText) {
        var input = document.getElementById(inputId);
        var startPos = input.selectionStart;
        var endPos = input.selectionEnd;
        input.value = input.value.substring(0, startPos) + insertedText + input.value.substring(endPos);
        var newCursorPos = startPos + insertedText.length;
        input.selectionStart = input.selectionEnd = newCursorPos;
        input.focus();
    };
})();

