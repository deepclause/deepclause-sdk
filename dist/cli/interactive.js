import * as readline from 'readline';
export function promptUser(prompt) {
    return new Promise((resolve) => {
        const rl = readline.createInterface({
            input: process.stdin,
            output: process.stdout,
        });
        console.log('\n' + '─'.repeat(60));
        console.log('📝 USER INPUT REQUIRED:');
        console.log('─'.repeat(60));
        console.log(prompt);
        console.log('─'.repeat(60));
        rl.question('Your response: ', (answer) => {
            rl.close();
            console.log('─'.repeat(60) + '\n');
            resolve(answer);
        });
    });
}
//# sourceMappingURL=interactive.js.map