# ProyectoLCC2022

> Un juego de captura de colores estilo *Flood It*, con una arquitectura híbrida poco convencional: interfaz en **React** + motor de reglas en **SWI-Prolog** vía **Pengines**.

Proyecto original de la carrera, actualizado para funcionar con herramientas modernas de Node y React, manteniendo intacta la lógica declarativa original del juego.

---

## Sobre el proyecto

ProyectoLCC2022 combina dos paradigmas de programación en una misma aplicación web:

- **Frontend en React**, encargado de la interacción visual y del estado de la partida.
- **Backend en Prolog (Pengines)**, que resuelve toda la lógica de captura, expansión de territorio y finalización del juego mediante programación lógica declarativa.

Más que un juego simple, es una muestra de cómo integrar un motor de reglas lógico con una interfaz moderna y reactiva.

## Cómo se juega

El juego funciona de forma similar a *Flood It*:

1. Antes de ver el tablero, elegís una celda de origen a ciegas.
2. Al entrar al tablero, aparece una grilla de celdas de colores y 6 opciones de color para elegir.
3. Tu selección inicial es la celda elegida junto con todas las celdas adyacentes del mismo color que ya formen parte de tu territorio.
4. Cada vez que elegís un nuevo color, tu territorio se expande incorporando todas las celdas adyacentes de ese color.
5. No hay una condición de "victoria": la partida continúa hasta que capturás el tablero completo, mostrando en todo momento la secuencia de colores jugada y la cantidad de turnos utilizados.
6. El juego incluye una ayuda de hasta 5 movimientos, que sugiere una posible secuencia para avanzar. Podés elegir si esa ayuda busca ser una secuencia óptima o no.

## Arquitectura

```
Frontend React  <-- HTTP / Pengines -->  Backend SWI-Prolog
(interfaz, UI)                            (lógica del juego)
```

## Tecnologías usadas

- React
- Create React App
- SWI-Prolog
- Pengines

## Requisitos

Antes de ejecutar el proyecto, asegurate de tener instalado:

- Node.js 18 o superior
- npm
- SWI-Prolog 10 o superior

## Ejecutar el proyecto

### 1) Clonar el repositorio

```bash
git clone git@github.com:AgusF22/ProyectoLCC2022.git
cd ProyectoLCC2022
```

### 2) Iniciar el servidor Prolog / Pengines

En una terminal, ejecutá:

```bash
cd pengines_server
swipl run.pl
```

Cuando el servidor se inicie por primera vez, SWI-Prolog te va a pedir un usuario y una contraseña para la interfaz administrativa. Podés ingresar cualquiera, por ejemplo:

- usuario: `admin`
- contraseña: `admin`

### 3) Iniciar el frontend React

En otra terminal, ejecutá:

```bash
cd ..
npm install
npm start
```

Luego abrí en el navegador:

```text
http://localhost:3000
```

### 4) Verificar el backend

El backend queda disponible en:

```text
http://localhost:3030/pengine/create
```

## Build de producción

Para generar una build lista para deploy:

```bash
npm run build
```

## Notas

- El frontend espera que el backend de Pengines esté corriendo en el puerto `3030`.
- Si el servidor Prolog no está disponible, la aplicación va a mostrar errores de conexión desde el cliente.

## Objetivo del proyecto

Este repositorio funciona tanto como pieza de software jugable como muestra de portfolio: un ejercicio de integración entre una interfaz moderna y un motor de reglas construido con programación lógica.