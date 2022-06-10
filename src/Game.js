import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import Square from './Square';

/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "#FF7547";
    case "v": return "#ED82ED";
    case "p": return "#FF93A3";
    case "g": return "#8AF2A1";
    case "b": return "#A3EBE1";
    case "y": return "#FFE175";
    default: return color; 
  }
 
}
class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      grid: null,
      complete: false,  // true if game is complete, false otherwise
      waiting: false,
      origin: undefined,
      clicks:[],
      capturadas:0,
      pe: '',
      StrategyBest: 0,
      listColors: [],
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.onOriginSelected = this.onOriginSelected.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
    this.handleHelp = this.handleHelp.bind(this);
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

  handleHelp(){
    // No action on click if game is complete or we are waiting.
    if (this.state.complete || this.state.waiting) {
      return;
    }
    
    const gridA = JSON.stringify(this.state.grid).replaceAll('"', "");
    const fila = this.state.origin ? this.state.origin[0] : 0;
    const col = this.state.origin ? this.state.origin[1] : 0;
    const PE = this.state.pe;
    const queryA = "ayuda(" + gridA + ","+fila+","+col+","+PE+",Best,List)";
    
    this.setState({
      waiting: true
    });

    this.pengine.query(queryA, (success, response) => {
      if (success) {
        
        this.setState({
          waiting: false,
          StrategyBest: response['Best'],
          listColors: response['List'],
        });
      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
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
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const length = gridS.split(",").length;
    const fila = this.state.origin ? this.state.origin[0] : 0;
    const col = this.state.origin ? this.state.origin[1] : 0;
    const queryS = "flick(" + gridS + "," + color + ", Grid, "+fila+","+col+",Capturadas)";
    
    if (!this.state.origin) {
      this.setState({
          origin: [0,0],
      })
    }   

    this.setState({
      waiting: true
    });

    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.state.clicks.push(color);
        this.setState({
          grid: response['Grid'],
          capturadas: response['Capturadas'],
          complete: response['Capturadas']===length,
          turns: this.state.turns + 1,
          waiting: false,
        });
      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
      }
    });
  }

  onOriginSelected(pos){
    this.setState({
      origin : pos
    })
  }


  render() {
    if (this.state.grid === null) {
      return null;
    }
    return (
      <div className="container">
        <div className="game">
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
            <div className="adyPanel">
              <div className="adyLab">Adyacents:</div>
              <div className="adyNum">{this.state.capturadas}</div>
              
            </div>

            <div className="strategyPanel">
              <div className="strategyLab">Strategy Depth:</div>
              <input type="number" min="1" max="8" onChange={e=>this.setState({
                  pe:e.target.value})}/> 
              <button
                  onClick={() => this.handleHelp()} name="Strategy Help" disabled={this.state.waiting} 
                  className={"StrategyHelp"}> Strategy Help 
              </button>
              <div className="BEST">{this.state.StrategyBest}</div>
              
            </div>


            <div className="colorsPanel">
              <div className="colorsLab">List:</div>
              <div className="colorsList">
                {this.state.listColors.map((colorS,i) => 
                   <Square
                    value={colorS}
                    key={i}
                    className={"clicksSquare"}
                  />
                  
                )}
              </div>
            </div>

            

          </div>
          <Board 
            grid={this.state.grid} 
            origin={this.state.origin} 
            onOriginSelected={!this.state.origin ? this.onOriginSelected : undefined}
          />

            <div className="EndGame">
              {this.state.complete===true ? <img src="win.jpeg" alt="MDN"></img> : ""}
            </div>

        </div>

        <div className="clicksPanel">
              <div className="clicksLab">History:</div>
              <div className="clicksNum">
                {this.state.clicks.map((colorS,i) => 
                   <Square
                    value={colorS}
                    key={i}
                    className={"clicksSquare"}
                  />
                  
                )}
              </div>
        </div>

      </div>
    );
  }
}

export default Game;