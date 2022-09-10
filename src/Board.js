import React from 'react';
import Square from './Square';

class Board extends React.Component {
    render() {
        return (
            <div className="board">
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            key={i + "." + j}
                            origin={this.props.originX === j && this.props.originY === i}
                        />
                    )
                )}
            </div>
        );
    }
}

export default Board;