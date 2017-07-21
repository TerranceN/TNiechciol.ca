window.addEventListener('load', function() {
  document.querySelectorAll(".expandable_code").forEach(function(code_block) {
    code_block.querySelector("a").addEventListener('click', function() {
      code_block.querySelector("pre").classList.toggle("hidden");
    });
  });
});
