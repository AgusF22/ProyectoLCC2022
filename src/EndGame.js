import React from 'react';

class EndGame extends React.Component {

    render() {
        if (this.props.end) {
            return (
                <div className='gameOverPanel'>
                    <div className='gameOverLab'>Game Over</div>
                    <button
                        className="startBtn"
                        onClick={() => window.location.reload()}
                    >Restart</button>
                </div>
            );
        } else {
            return (
                <div/>
            );
        }
    }
}

export default EndGame;