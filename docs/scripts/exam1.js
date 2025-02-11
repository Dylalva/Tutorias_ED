// examen.js

// Para almacenar el JSON de preguntas
let questionData = [];

// Al cargar la página, buscamos questions.json
window.addEventListener('DOMContentLoaded', () => {
  fetch('../src/questions/exam1.json')
    .then((response) => response.json())
    .then((data) => {
      questionData = data;
      renderQuestions(questionData);
    })
    .catch((error) => {
      console.error('Error al cargar JSON de preguntas:', error);
    });

  // Escuchamos el botón de finalizar
  const finishBtn = document.getElementById('finish-button');
  finishBtn.addEventListener('click', finishExam);
});

/**
 * Genera las preguntas dinámicamente y las inyecta en #questions-container
 */
function renderQuestions(questions) {
  const container = document.getElementById('questions-container');
  container.innerHTML = ''; // Limpia el contenedor antes de renderizar
  questions.forEach((q, index) => {
    // Creamos un elemento para la pregunta
    const questionElem = createQuestionElement(q, index + 1);
    container.appendChild(questionElem);
  });
}

/**
 * Crea el HTML para una pregunta determinada, basándose en su 'type'.
 * @param {Object} q - objeto con los datos de la pregunta
 * @param {number} questionNumber - número consecutivo de la pregunta (1,2,3,...)
 * @returns {HTMLElement} - el contenedor HTML de la pregunta
 */
function createQuestionElement(q, questionNumber) {
  const wrapper = document.createElement('div');
  wrapper.classList.add('question');
  
  // Título de la pregunta con su número
  const title = document.createElement('h3');
  title.textContent = `Pregunta #${questionNumber} - (${q.topic})`;
  wrapper.appendChild(title);

  // Texto de la pregunta
  const text = document.createElement('p');
  text.classList.add('question-text');
  text.textContent = q.question;
  wrapper.appendChild(text);

  // Dependiendo del tipo de pregunta, creamos la UI
  let inputArea;

  switch (q.type) {
    case 'single-choice':
      inputArea = createSingleChoice(q, questionNumber);
      break;
    case 'multiple-choice':
      inputArea = createMultipleChoice(q, questionNumber);
      break;
    case 'true-false':
      inputArea = createTrueFalse(q, questionNumber);
      break;
    case 'short-answer':
      inputArea = createShortAnswer(q, questionNumber);
      break;
    default:
      console.warn(`Tipo de pregunta desconocido: ${q.type}`);
  }
  
  wrapper.appendChild(inputArea);

  // Retroalimentación: la podemos ocultar inicialmente, o mostrarla luego
  const feedback = document.createElement('div');
  feedback.classList.add('feedback');
  feedback.textContent = q.feedback || '';
  // Ocultamos el feedback inicialmente
  feedback.style.display = 'none'; 
  wrapper.appendChild(feedback);

  return wrapper;
}

// --- Métodos para crear cada tipo de pregunta ---

function createSingleChoice(q, questionNumber) {
  const form = document.createElement('form');
  q.options.forEach((optionText, i) => {
    const label = document.createElement('label');
    const input = document.createElement('input');
    input.type = 'radio';
    input.name = `single-choice-${questionNumber}`; // agrupar las radios
    input.value = i; // el índice de la opción
    label.appendChild(input);
    label.append(` ${optionText}`);
    form.appendChild(label);
    form.appendChild(document.createElement('br'));
  });
  return form;
}

function createMultipleChoice(q, questionNumber) {
  const form = document.createElement('form');
  q.options.forEach((optionText, i) => {
    const label = document.createElement('label');
    const input = document.createElement('input');
    input.type = 'checkbox';
    input.name = `multiple-choice-${questionNumber}`;
    input.value = i;
    label.appendChild(input);
    label.append(` ${optionText}`);
    form.appendChild(label);
    form.appendChild(document.createElement('br'));
  });
  return form;
}

function createTrueFalse(q, questionNumber) {
  const form = document.createElement('form');
  ['true', 'false'].forEach((val) => {
    const label = document.createElement('label');
    const input = document.createElement('input');
    input.type = 'radio';
    input.name = `true-false-${questionNumber}`;
    input.value = val;
    label.appendChild(input);
    label.append(` ${val === 'true' ? 'Verdadero' : 'Falso'}`);
    form.appendChild(label);
    form.appendChild(document.createElement('br'));
  });
  return form;
}

function createShortAnswer(q, questionNumber) {
  const form = document.createElement('form');
  const input = document.createElement('input');
  input.type = 'text';
  input.name = `short-answer-${questionNumber}`;
  input.placeholder = 'Escribe tu respuesta...';
  form.appendChild(input);
  return form;
}

// --- Lógica para terminar el examen y mostrar calificación ---

function finishExam() {
  let correctCount = 0;
  let totalCount = questionData.length;

  // Contabilizar la nota por tema
  let topicsScore = {};   // { 'Recursividad Básica': { correct: 0, total: 0 }, ... }

  // Iterar las preguntas
  questionData.forEach((q, index) => {
    const questionNumber = index + 1;
    const isCorrect = evaluateQuestion(q, questionNumber);

    // Actualizamos correctCount
    if (isCorrect) correctCount++;

    // Manejar la nota por tema
    if (!topicsScore[q.topic]) {
      topicsScore[q.topic] = { correct: 0, total: 0 };
    }
    topicsScore[q.topic].total += 1;
    if (isCorrect) {
      topicsScore[q.topic].correct += 1;
    }

    // Mostrar feedback (puede ser opcional)
    // Seleccionamos el elemento .feedback correspondiente
    const questionElem = document.querySelectorAll('.question')[index];
    const feedbackElem = questionElem.querySelector('.feedback');
    feedbackElem.style.display = 'block'; 
    // Opcional: podrías colorear feedback según correcto o incorrecto
    feedbackElem.style.color = isCorrect ? 'green' : 'red';
  });

  // Calcular nota final
  const finalScore = Math.round((correctCount / totalCount) * 100);
  
  // Construir un mensaje con la nota general y por tema
  let message = `Has obtenido ${correctCount} de ${totalCount} preguntas correctas.\n`;
  message += `Nota final: ${finalScore}%\n\n`;

  message += 'Nota por tema:\n';
  for (let topic in topicsScore) {
    const tCorrect = topicsScore[topic].correct;
    const tTotal = topicsScore[topic].total;
    const tScore = Math.round((tCorrect / tTotal) * 100);
    message += `- ${topic}: ${tCorrect}/${tTotal} (${tScore}%)\n`;
  }

  // Mostrar la ventana con la nota
  alert(message);
}

function evaluateQuestion(q, questionNumber) {
  switch (q.type) {
    case 'single-choice': {
      const selected = document.querySelector(`input[name="single-choice-${questionNumber}"]:checked`);
      if (!selected) return false; // No hay respuesta
      const userValue = parseInt(selected.value, 10);
      return userValue === q.correctAnswer;
    }
    case 'multiple-choice': {
      // Tomamos todos los checkboxes marcados
      const checkboxes = document.querySelectorAll(`input[name="multiple-choice-${questionNumber}"]:checked`);
      if (checkboxes.length === 0) return false; 
      // Recogemos los valores en un array de índices (number)
      const userValues = Array.from(checkboxes).map(cb => parseInt(cb.value, 10));
      
      // Comparamos con el array de correctAnswers
      // Deben coincidir en largo y en elementos
      if (userValues.length !== q.correctAnswers.length) return false;
      // Ver si todos los índices del usuario están en correctAnswers
      return userValues.every(val => q.correctAnswers.includes(val));
    }
    case 'true-false': {
      const selected = document.querySelector(`input[name="true-false-${questionNumber}"]:checked`);
      if (!selected) return false; 
      return selected.value === q.correctAnswer;
    }
    case 'short-answer': {
      const input = document.querySelector(`input[name="short-answer-${questionNumber}"]`);
      if (!input) return false;
      // Comparamos texto (podrías normalizar mayúsculas/minúsculas)
      return input.value.trim().toLowerCase() === q.correctAnswer.trim().toLowerCase();
    }
    default:
      return false;
  }
}
