window.addEventListener('DOMContentLoaded', () => {
    const container = document.getElementById('components-container');
  
    // Cargamos el archivo externo del componente:
    fetch('../components/component-true-false.html')
      .then(response => response.text())
      .then(html => {
        // Insertamos el HTML en el contenedor
        container.innerHTML = html;
  
        // Si quieres cargar varios componentes, puedes encadenar más fetch
        // o crear un array de URLs de componentes y recorrerlo.
      })
      .catch(error => {
        console.error('Error al cargar el componente:', error);
      });
  });
  