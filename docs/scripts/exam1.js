// Array de preguntas con distinto 'type'
const questions = [
    {
      type: 'truefalse',
      question: '¿La palabra "recursividad" se refiere a una función que se llama a sí misma?',
      correctAnswer: 'true'
    },
    {
      type: 'multipleChoice',
      question: '¿Cuál de estas estructuras se utiliza para modelar estados y transiciones?',
      options: ['Árboles', 'Máquinas de estado finito', 'Listas enlazadas', 'Recursividad'],
      correctAnswer: 'Máquinas de estado finito'
    },
    {
      type: 'shortAnswer',
      question: 'Escribe un ejemplo de lenguaje de programación funcional:',
      correctAnswer: 'Haskell'
    }
  ];
  
  // Al cargar la página, creamos los elementos de pregunta en el DOM.
  window.addEventListener('DOMContentLoaded', () => {
    const container = document.getElementById('questions-container');
  
    // Recorremos cada pregunta y creamos su representación en HTML
    questions.forEach((q) => {
      const questionElement = createQuestionElement(q);
      container.appendChild(questionElement);
    });
  });
  
  /**
   * Crea un elemento <div> con la estructura de la pregunta en función de su tipo
   * @param {Object} q - Objeto que describe la pregunta
   * @returns {HTMLElement} - Elemento contenedor de la pregunta
   */
  function createQuestionElement(q) {
    // Contenedor principal de la pregunta
    const wrapper = document.createElement('div');
    wrapper.classList.add('question-wrapper');
  
    // Título (enunciado)
    const title = document.createElement('h2');
    title.textContent = q.question;
    wrapper.appendChild(title);
  
    // Dependiendo del tipo de pregunta, creamos la estructura
    if (q.type === 'truefalse') {
      // Verdadero/Falso: dos inputs radio
      const trueLabel = document.createElement('label');
      trueLabel.innerHTML = `
        <input type="radio" name="${q.question}" value="true" /> Verdadero
      `;
      
      const falseLabel = document.createElement('label');
      falseLabel.innerHTML = `
        <input type="radio" name="${q.question}" value="false" /> Falso
      `;
      
      wrapper.appendChild(trueLabel);
      wrapper.appendChild(falseLabel);
  
    } else if (q.type === 'multipleChoice') {
      // Opción múltiple: varias opciones (radio)
      q.options.forEach((option) => {
        const optionLabel = document.createElement('label');
        optionLabel.innerHTML = `
          <input type="radio" name="${q.question}" value="${option}" /> ${option}
        `;
        wrapper.appendChild(optionLabel);
      });
  
    } else if (q.type === 'shortAnswer') {
      // Respuesta corta: un input de texto
      const input = document.createElement('input');
      input.type = 'text';
      input.placeholder = 'Tu respuesta...';
      wrapper.appendChild(input);
    }
  
    return wrapper;
  }
  