import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';
import EndGame from './EndGame';

/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  var toReturn;
  switch (color) {
    case "r": toReturn = "#FF6961"; break;
    case "v": toReturn = "#B19CD8"; break;
    case "p": toReturn = "#FFD1DC"; break;
    case "g": toReturn = "#8FE381"; break;
    case "b": toReturn = "#78C5DC"; break;
    case "y": toReturn = "#FDFD96"; break;
    default: toReturn = "white";
  }
  return toReturn;
}

class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      grid: null,
      captured: 0,
      originX: 0,
      originY: 0,
      gameStarted: false,
      history: [],
      complete: false,  // true if game is complete, false otherwise
      waiting: false
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }

  handlePengineCreate() {
    const queryS = 'init(Grid)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid']
        });
      }
    });
  }

  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],cell(0,0,g),r,Captured,Grid,Fin)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const X = this.state.originX;
    const Y = this.state.originY;
    const queryS = "flick(" + gridS + ",cell(" + X + "," + Y + "," + this.state.grid[X][Y] + ")," + color + ", Captured, Grid, Fin)";
    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.state.history.unshift(color);
        this.setState({
          grid: response['Grid'],
          turns: this.state.turns + 1,
          captured: response['Captured'],
          complete: response['Fin'],
          waiting: false
        });
      } else {
        // Prolog query will fail when the clicked color coincides with that in the origin cell.
        this.setState({
          waiting: false
        });
      }
    });
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }
    if (!this.state.gameStarted) {
      return (
        <div className="game">
          <div className="originSelectPanel">
            <div className="chooseCellLab">Choose origin cell:</div>
            <div className="coordSelectPanel">
              <div className="turnsLab">X:</div>
              <select className="selectBox" name="select" onChange={e => this.setState({ originX: e.target.value })}>
                {Array.from({ length: 14 }, (x, i) => i).map(j =>
                  <option key={"x" + j}>{j}</option>
                )}
              </select>
            </div>
            <div className="coordSelectPanel">
              <div className="turnsLab">Y:</div>
              <select className="selectBox" name="select" onChange={e => this.setState({ originY: e.target.value })}>
                {Array.from({ length: 14 }, (x, i) => i).map(j =>
                  <option key={"y" + j}>{j}</option>
                )}
              </select>
            </div>
            <button
              className="startBtn"
              onClick={() => this.setState({ gameStarted: true })}
            >Start</button>
          </div>
        </div>
      );
    }
    return (
      <div className="game">
        <div className="topPanel">
          <div className="leftPanel">
            <div className="buttonsPanel">
              {colors.map(color =>
                <button
                  className="colorBtn"
                  style={{ backgroundColor: colorToCss(color) }}
                  onClick={() => this.handleClick(color)}
                  key={color}
                />)}
            </div>
            <div className="turnsPanel">
              <div className="turnsLab">Turns</div>
              <div className="turnsNum">{this.state.turns}</div>
            </div>
            <div className="capturedPanel">
              <div className="capturedLab">Captured</div>
              <div className="capturedNum">{this.state.captured}</div>
            </div>
            <EndGame end={this.state.complete}/>
          </div>
          <Board grid={this.state.grid}/> 
        </div>
        <div className="bottomPanel">
          <div className="historyLab">History</div>
          <div className="historyPanel">
            {this.state.history.map((color, i) =>
              <Square
                value={color}
                key={color + i}
              />)}
          </div>
        </div>
      </div>
    );
  }
}

export default Game;